package dire

import dire.control.{RawSignal ⇒ RS, Reactor, Const}
import scala.reflect.runtime.universe.TypeTag
import scalaz._, Scalaz._, effect.IO
import scalaz.Isomorphism.<~>
import scalaz.Leibniz.===

/** Represents a transformation from an input signal or 
  * event stream to an output signal or event stream.
  *
  * This class is the main entry point into reactive programming
  * with dire. It offers a rich set of combinators needed to describe
  * reactive networks.
  *
  * @tparam A  Type of events fired by the input signal or event stream
  * @tparam B  Type of events fired by the output signal or event stream
  * @tparam I  Container type that holds input events. This is either
  *            `Id` for signals or `Event` for event streams
  * @tparam O  Container type that holds output events. This is either
  *            `Id` for signals or `Event` for event streams
  */
class RF[-A,+B,I[+_],O[+_]] private[dire](
    private[dire] val run: (RS[I[A]], Reactor) ⇒ IO[RS[O[B]]])
    (implicit private[dire] val MI: IdOrEvent[I],
              private[dire] val MO: IdOrEvent[O]) {
  import RF._, DataSink._

  /** Applies a time-changing function to the signals values */
  def ap[C,AA<:A](f: RF[AA,B ⇒ C,I,O]): RF[AA,C,I,O] =
    (f <*> this)(_ apply _)

  /** Sequentially combines two signal functions */
  def andThen[C,O1[+_]](that: RF[B,C,O,O1]): RF[A,C,I,O1] =
    that compose this

  /** Like `syncTo` but performs side effects asynchronuously,
    * that is, the main `Reactor` thread is not blocked.
    *
    * The reactive system guarantees that, as long as the given
    * `out` is not used as a data sink for other signals, it will
    * only be accessed sequentially from this signal, i.e. it
    * need not be thread safe. It is for instance possible to
    * write all values of this signal to a file without being
    * afraid of race conditions or other ugliness. If the same
    * file is used to monitor several distinct signals or
    * event streams they should either be merged in a single
    * signal (event stream) first, or function `to` should
    * be used which guarantees that all access from a given
    * reactive graph will be sequential.
    */
  def asyncTo(out: Out[B]): RF[A,B,I,O] = to(DataSink async out)

  /** Connect a reactive branch to this signal function but
    * return to the original branch afterwards.
    */
  def branch[C,O1[+_]](that: RF[B,C,O,O1]): RF[A,B,I,O] = 
    RF { (ra,r) ⇒ run(ra, r) >>= { rb ⇒ that.run(rb,r) as rb } }

  /** Creates an event stream that fires whenever this signal's
    * value changes to a new one that is distinct from the old
    * value.
    *
    * The resulting event stream starts with this signal's initial value
    */
  def changes[BB>:B:Equal](implicit W: AsId[O])
    : RF[A,BB,I,Event] = sfTo(this).distinct[BB].ef

  //Used in unit tests
  private[dire] def changeTo(out: Out[Change[O[B]]]): RF[A,B,I,O] =
    toO(DataSink syncC out)

  /** Map and filter an event stream in one run */
  def collect[C](f: B ⇒ Option[C])
                (implicit W: AsEv[O]): RF[A,C,I,Event] =
    sync1(efTo(this))(_.v collect f)((ceb,_) ⇒ ceb.v collect f)

  /** Sequentially combines two signal functions */
  def compose[C,O1[+_]](that: RF[C,A,O1,I]): RF[C,B,O1,O] =
    RF[C,B,O1,O]((rc, r) ⇒ that.run(rc,r) >>= { run(_, r) })(that.MI,MO)

  /** Contravariant mapping */
  def contramap[C](f: C ⇒ A): RF[C,B,I,O] = compose(RF.id[C,I] map f)

  /** Counts the number of events this event stream fired and
    * stores the results in a signal
    */
  def count(implicit W: AsEv[O]): RF[A,Int,I,Id] = scanMap { _ ⇒ 1 }

  /** Returns a signal that only fires an event if its new
    * value is different from its old one
    */
  def distinct[BB>:B](implicit E: Equal[O[BB]]): RF[A,BB,I,O] =
    sync1O(this)(_.v)(
      (cb,cc) ⇒ if ((cb.v: O[BB]) ≟ cc.v) None else Some(cb.v))

  /** Creates an event stream that fires whenever this signal
    * fires an event.
    *
    * This is similar to `changes` but fires even if the signal's
    * new value is not distinct from the old one. Note that unlike
    * `events` the first event will be fired at `T0` with the
    * signal's initial value.
    */
  def ef(implicit W: AsId[O]): RF[A,B,I,Event] = {
    def f(c: Change[B]): Event[B] = Once(c.v)

    sync1(sfTo(this))(f)((cb,_) ⇒ f(cb))
  }

  /** Creates an event stream that fires whenever this signal
    * fires an event.
    *
    * Note that unlike with function `ef`, the resulting event stream
    * will skip the signal's initial value.
    */
  def events(implicit W: AsId[O]): RF[A,B,I,Event] = 
    sync1[A,B,B,I,Id,Event](sfTo(this))(_ ⇒ Never)((cb,_) ⇒ Once(cb.v))


  /** Filters an event stream according to the given predicate */
  def filter(p: B ⇒ Boolean)(implicit W: AsEv[O]): RF[A,B,I,Event] =
    collect[B] { b ⇒ p(b) option b }

  /** Converts this event stream to a signal with initial value
    * `ini`
    */
  def hold[C>:B](ini: C)(implicit W: AsEv[O]): RF[A,C,I,Id] = 
    scan[C](ini)((next,_) ⇒ next)

  /** Functor map */
  def map[C](f: B ⇒ C): RF[A,C,I,O] =
    sync1(this)(_.v map f)((cb,_) ⇒ cb.v map f)

  /** Returns an event stream that fires whenever one of the input streams
    * fire.
    *
    * If both event streams fire at the same time, the event of the
    * second (right) stream is ignored
    */
  def merge[AA<:A,BB>:B](that: RF[AA,BB,I,Event])
                        (implicit W: AsEv[O]): RF[AA,BB,I,Event] = {
    def later(c1: Change[Event[BB]], c2: Change[Event[BB]]) =
      (c1.v, c2.v) match {
        case (Once(x), Once(y)) ⇒ if (c1.at >= c2.at) Once(x) else Once(y)
        case (ox, oy)           ⇒ ox orElse oy
      }

    sync2(efTo(this),that)(later)((ca,cb,_) ⇒ later(ca,cb))
  }

  /** Returns an event stream that fires this signals actual value
    * whenever the given event stream fires
    */
  def on[C,AA<:A](ef: RF[AA,C,I,Event])
                 (implicit W: AsId[O])
                 : RF[AA,B,I,Event] = upon(ef)((b,_) ⇒ b)

  /** Accumulates events fired by this event stream in a signal
    *
    * If the original event stream fires its first event `a` at time
    * `T0`, the resulting signal's initial value will be
    * `next(a, ini)`, otherwise it will be just `ini`
    *
    * This is the most fundamental function for accumulating the
    * events of an event stream in a signal. All other functions like
    * `hold`, `scanMap`, and so on can be derived from this one
    *
    * @param ini  the initial value of the resulting signal
    * @param next combines the fired event with the
    *             value accumulated so far
    */
  def scan[C](ini: ⇒ C)(next: (B,C) ⇒ C)(implicit W: AsEv[O])
    : RF[A,C,I,Id] = {
      def in(cb: Change[Event[B]]) = cb.v.fold(next(_, ini), ini)

      def ne(cb: Change[Event[B]], cc: Change[C]) =
        cb.v.fold(next(_, cc.v), cc.v)

      sync1[A,B,C,I,Event,Id](efTo(this))(in)(ne)
    }

  /** Accumulates events by first transferring them to a value
    * with a Monoid instance
    */
  def scanMap[C:Monoid](f: B ⇒ C)(implicit W: AsEv[O]): RF[A,C,I,Id] = 
    scan(∅[C])((a,b) ⇒ b ⊹ f(a))

  /** Accumulates events in a container */
  def scanPlus[F[+_]](implicit P: ApplicativePlus[F], W: AsEv[O])
    : RF[A,F[B],I,Id] = scanMap(_.η[F])(P.monoid, W)

  /** Accumulates the results of a stateful calculation */
  def scanSt[S,C](ini: S)(implicit WS: B <:< State[S,C], W: AsEv[O])
    : RF[A,(S,C),I,Event] = {
    def sfE = scan[Event[(S,C)]](Never)((s: B, e: Event[(S,C)]) ⇒ 
      e map { s run _._1 } orElse Once(s run ini))

    RF[A,(S,C),I,Event](sfE.run)
  }

  /** Accumulates the results of a stateful calculation in a signal
    * starting at value `ini`.
    *
    * Note that the stateful calculation is performed purely for its
    * effects on the state and the result is discarded.
    */
  def scanStHold[S,C]
    (ini: S)
    (implicit WS: B <:< State[S,C], W: AsEv[O]): RF[A,S,I,Id] =
    scanStS[S,C](ini) hold ini

  /** Accumulates the results of a stateful calculation 
    * keeping the state and discarding the result
    */
  def scanStS[S,C]
    (ini: S)
    (implicit WS: B <:< State[S,C], W: AsEv[O]): RF[A,S,I,Event] =
    scanSt[S,C](ini) map { _._1 }

  /** Accumulates the results of a stateful calculation 
    * discarding the new state
    */
  def scanStV[S,C](ini: S)
    (implicit WS: B <:< State[S,C], W: AsEv[O]): RF[A,C,I,Event] =
    scanSt[S,C](ini) map { _._2 }

  /** Accumulates events using a Monoid */
  def sum[BB>:B](implicit M: Monoid[BB], W: AsEv[O]): RF[A,BB,I,Id] = 
    scanMap[BB](identity)

  /** Performs the given IO-action with this signal's initial value
    * and whenever this signal changes.
    *
    * Note that `out` will be called by the `Reactor`'s
    * running thread that updates
    * the main reactive graph. Therefore, side effects performed
    * by `out` need to be fast if the system should stay reactive.
    * Consider using `asyncTo` instead for fully asynchronuous
    * side effects or when side effects have to be performed in a
    * special type of thread (the Swing event dispatch thread for instance).
    */
  def syncTo(out: Out[B]): RF[A,B,I,O] = to(DataSink sync out)

  /** Asynchronuously output the values of this signal to a data sink
    *
    * How the data sink operates and what concurrency strategy is
    * applied is defined in the [[dire.DataSink]] type class.
    */
  def to(sink: DataSink[B]): RF[A,B,I,O] = toO(sink.lift)

  private def toO(s: DataSink[O[B]]): RF[A,B,I,O] = 
    RF { (ra,r) ⇒ run(ra,r) >>= r.sink(s) }

  /** Combines a signal with an event stream through a
    * function of arity two.
    *
    * The resulting event stream fires only, when the given
    * event stream fires.
    */
  def upon[C,D,AA<:A](ef: RF[AA,C,I,Event])
                     (f: (B,C) ⇒ D)
                     (implicit W: AsId[O]): RF[AA,D,I,Event] = {
    def g(cb: Change[B], cec: Change[Event[C]]): Option[Event[D]] =
      if (cec.at >= cb.at) Some(cec.v map { f(cb.v,_) } )
      else None

    def ini(cb: Change[B], cec: Change[Event[C]]) = g(cb,cec) | Never

    sync2(sfTo(this),ef)(ini)((cb,cec,_) ⇒ ini(cb,cec))
  }

  /** Alias for `andThen` */
  def >=>[C,O1[+_]](that: RF[B,C,O,O1]): RF[A,C,I,O1] = andThen(that)

  /** Alias for `compose` */
  def <=<[C,O1[+_]](that: RF[C,A,O1,I]): RF[C,B,O1,O] = compose(that)

  /** Combines two signals with a pure function */
  def <*>[C,D,AA<:A](that: RF[AA,C,I,O])(f: (B,C) ⇒ D): RF[AA,D,I,O] =
    sync2(this, that)((cb,cc) ⇒ ^(cb.v, cc.v)(f))((cb,cc,_) ⇒ ^(cb.v, cc.v)(f))

  /** Alias for `contramap`*/
  def ∙ [C](f: C ⇒ A): RF[C,B,I,O] = contramap(f)

  /** Alias for `asyncTo` */
  def -/->(out: Out[B]): RF[A,B,I,O] = asyncTo(out)

  /** Alias for `syncTo` */
  def -->(out: Out[B]): RF[A,B,I,O] = syncTo(out)

  /** Alias for `to` */
  def ==>(sink: DataSink[B]): RF[A,B,I,O] = to(sink)

  /** Appends a signal or event source to this signal function
    *
    * The resulting signal function will run both, the original
    * and the new signal function. This is useful to concatenate
    * completely unrelated sources
    */
  def >>[C,O1[+_]](that: RF[⊥,C,Event,O1]): RF[A,C,I,O1] =
    RF[A,C,I,O1]((ra,r) ⇒
      run(ra,r) >> that.run(Const(Never),r))(MI,that.MO)
}

object RF extends RFFunctions with RFInstances {

  type AsId[O[+_]] = O[Any] === Id[Any]

  type AsEv[O[+_]] = O[Any] === Event[Any]

  private def sfTo[A,B,I[+_],O[+_]:AsId](sf: RF[A,B,I,O])
    : RF[A,B,I,Id] = sf.asInstanceOf[RF[A,B,I,Id]]

  private def efTo[A,B,I[+_],O[+_]:AsEv](sf: RF[A,B,I,O])
    : RF[A,B,I,Event] = sf.asInstanceOf[RF[A,B,I,Event]]

  private[dire] def apply[A,B,I[+_]:IdOrEvent,O[+_]:IdOrEvent]
    (run: (RS[I[A]], Reactor) ⇒ IO[RS[O[B]]]): RF[A,B,I,O] =
    new RF(run)

  private[dire] def ein[A]
    (f: Reactor ⇒ IO[RS[Event[A]]]): EIn[A] =
    apply[⊥,A,Event,Event]((_,r) ⇒ f(r))

  private[dire] def sin[A]
    (f: Reactor ⇒ IO[RS[A]]): SIn[A] =
    apply[⊥,A,Event,Id]((_,r) ⇒ f(r))

  private[dire] lazy val processors =
    Runtime.getRuntime.availableProcessors
}

trait RFFunctions {
  lazy val ms = 1000L

  lazy val s = ms * 1000L

  lazy val min = s * 60L

  lazy val h = min * 60L

  // ***                  *** //
  // *** Constant signals *** //
  // ***                  *** //


  private[dire] def constRF[A,B,I[+_]:IdOrEvent,O[+_]:IdOrEvent]
    (b: ⇒ O[B]): RF[A,B,I,O] = RF[A,B,I,O]((_,_) ⇒ RS const b)

  /** A constant signal that never changes */
  def const[A](a: ⇒ A): SIn[A] = constRF[⊥,A,Event,Id](a)

  /** The empty event stream that never fires an event */
  def never[A]: EIn[A] = constRF[⊥,A,Event,Event](Never)

  /** An event stream that fires only once at time zero */
  def now[A](a: ⇒ A): EIn[A] = constRF[⊥,A,Event,Event](Once(a))


  // ***                                            *** //
  // *** Basic signal and event stream transformers *** //
  // ***                                            *** //

  /** Event stream function from a pure function */
  def ef[A,B](f: A ⇒ B): EF[A,B] = idE map f

  /** The identity signal function */
  def idS[A]: SF[A,A] = id

  /** The identity event stream function */
  def idE[A]: EF[A,A] = id

  /** Signal function from a pure function */
  def sf[A,B](f: A ⇒ B): SF[A,B] = idS map f
  
  /** Asynchronously runs the given IO action whenever the
    * input event stream fires.
    *
    * The resulting event stream fires its own even, whenever
    * the result of `f` is ready.
    */
  def efIO[A,B](f: A ⇒ IO[B]): EF[A,B] =
    RF[A,B,Event,Event] { (ea,r) ⇒ r.trans(f)(ea) }

  /** Asynchronuously loops back the output of the given
    * event stream to its input
    */
  def loop[A](ef: EF[A,A]): EIn[A] = 
    RF[A,A,Event,Event] { (ea,r) ⇒ r.loop[Event[A]](Never)(ef.run) }

  // ***                   *** //
  // *** Sources and Sinks *** //
  // ***                   *** //

  /** Creates an event source from an external data source
    *
    * Unlike `eventSrc`, the resulting signal function is cached using `s`
    * as a key.
    */
  def cachedEventSrc[S,V:TypeTag](s: S)(implicit Src: DataSource[S,Event[V]])
    : EIn[V] = cached[⊥,V,Event,Event](eventSrc(s), s)

  /** Creates an input Signal from an external data source
    *
    * Unlike `signalSrc`, the resulting signal function is cached using `s`
    * as a key.
    */
  def cachedSignalSrc[S,V:TypeTag](s: S)(implicit Src: DataSource[S,V])
    : SIn[V] = cached[⊥,V,Event,Id](signalSrc(s), s)

  /** Creates an event source from an external data source
    *
    * See the [[dire.DataSource]] type class for more details
    */
  def eventSrc[S,V](s: S)(implicit Src: DataSource[S,Event[V]]): EIn[V] =
    RF ein { _.source(Src ini s)(Src cb s) }

  /** Creates an input Signal from an external data source
    *
    * See the [[dire.DataSource]] type class for more details
    */
  def signalSrc[S,V](s: S)(implicit Src: DataSource[S,V]): SIn[V] =
    RF sin { _.source(Src ini s)(Src cb s) }

  /** Asynchronuously fires the given event once */
  def once[A](a: ⇒ A): EIn[A] = eventSrc(())(DataSource once a)

  // ***                *** //
  // *** Cached signals *** //
  // ***                *** //

  /** Sometimes, part of a reactive graph appears in several
    * places in the description of the reactive graph.
    *
    * Caching such a sub-graph using the given `tag` guarantees,
    * that the result of the given signal function with the
    * same type of input and output parameters and the same `tag`
    * is only created once when setting up the reactive graph.
    * Additional calls to the resulting signal-functions `run`
    * method will result in the cached `RawSignal` being returned.
    */
  def cached[A,B,I[+_],O[+_]]
    (sf: RF[A,B,I,O], tag: Any)
    (implicit TI: TypeTag[I[A]], TO: TypeTag[O[B]]): RF[A,B,I,O] =
    RF[A,B,I,O]((ra,r) ⇒ r.cached[I[A],O[B]](sf.run, tag)(ra))(
      sf.MI,sf.MO)


  // ***              *** //
  // *** Time signals *** //
  // ***              *** //

  /** The time signal
    *
    * In every isolated reactive system there is only one such signal
    */
  def time: SIn[Time] = RF.sin { _.timeSignal }

  /** Time signal that starts at 0 and updates every second */
  def seconds: SIn[Int] = time.events filter { _ % s == 0L } count

  /** Time signal that starts at 0 and updates every minute */
  def minutes: SIn[Int] = time.events filter { _ % min == 0L } count

  /** Time signal that starts at 0 and updates every hour */
  def hours: SIn[Int] = time.events filter { _ % h == 0L } count

  /** An asynchronous event source that fires at regular
    * intervals.
    *
    * This is very useful as a basic source of events to simulate
    * all kinds of real time applications.
    * Note that an arbitrary number of completely independant
    * event streams can thus be created. 
    */
  def ticks(step: Time): EIn[Unit] = eventSrc[Time,Unit](step)


  // ***                          *** //
  // *** Running signal functions *** //
  // ***                          *** //

  /** Sets up a reactive network and runs it until the given
    * abort condition is fullfilled.
    *
    * Note that the calling thread is blocked until the abbort
    * condition is fullfilled and the reactive system shutdown
    *
    * @param in  The signal function that describes the reactive
    *            network
    *
    * @param step  Time resolution in microseconds. If the given
    *              signal function depends on the `Time` signal
    *              this number denotes how frequently `Time` is
    *              being updated. The default value is `1000L`
    *              (one millisecond)
    *
    * @param proc Number of processors (threads) available to the
    *             actors running in the background. The default
    *             value is the total number of processors of the
    *             system the application is running on. Note that
    *             a minimum of two threads is chosen if `proc` is
    *             less than two.
    *
    * @param stop This function should return `true` when a certain
    *             abbort condition is fulfilled. In that case, the
    *             reactive framework will cease to run and release
    *             all its resources. Note that the framework will
    *             stop immediately AFTER `stop` has returned true
    *             but that the event that lead to abortion
    *             is still processed and passed to all
    *             registered data sinks.
    */
  def runS[A]
    (in: SIn[A], proc: Int = RF.processors, step: Time = 1000L)
    (stop: A ⇒ Boolean)
    : IO[Unit] =
    run(in, proc, step)(stop)

  /** Sets up a reactive network and runs it until the given
    * abort condition is fullfilled.
    *
    * Note that the calling thread is blocked until the abbort
    * condition is fullfilled and the reactive system shutdown
    *
    * @param in  The event source that describes the reactive
    *            network
    *
    * @param step  Time resolution in microseconds. If the given
    *              signal function depends on the `Time` signal
    *              this number denotes how frequently `Time` is
    *              being updated. The default value is `1000L`
    *              (one millisecond)
    *
    * @param proc Number of processors (threads) available to the
    *             actors running in the background. The default
    *             value is the total number of processors of the
    *             system the application is running on. Note that
    *             a minimum of two threads is chosen if `proc` is
    *             less than two.
    *
    * @param stop This function should return `true` when a certain
    *             abbort condition is fulfilled. In that case, the
    *             reactive framework will cease to run and release
    *             all its resources. Note that the framework will
    *             stop immediately AFTER `stop` has returned true
    *             but that the event that lead to abortion
    *             is still processed and passed to all
    *             registered data sinks.
    */
  def runE[A]
    (in: EIn[A], proc: Int = RF.processors, step: Time = 1000L)
    (stop: A ⇒ Boolean)
    : IO[Unit] =
    run[A,Event](in, proc, step)(stop)


  // ***                                   *** //
  // ***  package private helper functions *** //
  // ***                                   *** //

  private[dire] def id[A,F[+_]:IdOrEvent]: RF[A,A,F,F] = RF((a,_) ⇒ IO(a))

  /** Creates a derrived signal depending on two input signals
    * that is synchronously updated whenever one of the two
    * input signals changes
    *
    * @param sa the first of the two signal functions to be
    *           combined
    *
    * @param sa the second of the two signal functions to be
    *           combined
    *
    * @param ini  calculates the initial value of the new
    *             signal from the two input signals' initial
    *             values
    *
    * @param next  calculates the new value of the derived
    *              signal from the two actual values of the
    *              input signals and the derived signal's
    *              latest value
    */
  private[dire] def sync2[R,A,B,C,I[+_]:IdOrEvent,O1[+_],O2[+_],O3[+_]:IdOrEvent]
    (sa: RF[R,A,I,O1], sb: RF[R,B,I,O2])
    (ini: (Change[O1[A]], Change[O2[B]]) ⇒ O3[C])
    (next: (Change[O1[A]], Change[O2[B]], Change[O3[C]]) ⇒ O3[C])
    : RF[R,C,I,O3] = sync2O(sa, sb)(ini)((ca,cb,cc) ⇒ isNeverToO(next(ca,cb,cc)))

  private[dire] def sync2O[R,A,B,C,I[+_]:IdOrEvent,O1[+_],O2[+_],O3[+_]:IdOrEvent]
    (sa: RF[R,A,I,O1], sb: RF[R,B,I,O2])
    (ini: (Change[O1[A]], Change[O2[B]]) ⇒ O3[C])
    (next: (Change[O1[A]], Change[O2[B]], Change[O3[C]]) ⇒ Option[O3[C]])
    : RF[R,C,I,O3] =
    RF[R,C,I,O3]((rr, r) ⇒ 
      for {
        ra ← sa.run(rr, r)
        rb ← sb.run(rr, r)
        rc ← RS.sync2(ra, rb)(ini)(next)
      } yield rc
    )

  /** Creates a derrived signal depending on one input signal
    * that is synchronously updated whenever the
    * input signal changes
    *
    * @param sa   the input signal functions
    *
    * @param ini  calculates the initial value of the new
    *             signal from the input signal's initial
    *             value
    *
    * @param next  calculates the new value of the derived
    *              signal from the actual value of the
    *              input signal and the derived signal's
    *              latest value
    */
  private[dire] def sync1[R,A,B,I[+_]:IdOrEvent,O1[+_],O2[+_]:IdOrEvent]
    (sa: RF[R,A,I,O1])
    (ini: Change[O1[A]] ⇒ O2[B])
    (next: (Change[O1[A]], Change[O2[B]]) ⇒ O2[B])
    : RF[R,B,I,O2] = sync1O(sa)(ini)((ca,cb) ⇒ isNeverToO(next(ca,cb)))

  private[dire] def sync1O[R,A,B,I[+_]:IdOrEvent,O1[+_],O2[+_]:IdOrEvent]
    (sa: RF[R,A,I,O1])
    (ini: Change[O1[A]] ⇒ O2[B])
    (next: (Change[O1[A]], Change[O2[B]]) ⇒ Option[O2[B]]): RF[R,B,I,O2] =
    RF[R,B,I,O2]((rr, r) ⇒ 
      for {
        ra ← sa.run(rr, r)
        rb ← RS.sync1(ra)(ini)(next)
      } yield rb
    )

  private[dire] def isNeverToO[F[+_]:IdOrEvent,A](f: F[A]): Option[F[A]] =
    if (IdOrEvent[F] isNever f) None else Some(f)

  private[dire] def run[A,O[+_]]
    (in: RF[⊥,A,Event,O], proc: Int, step: Time)
    (stop: A ⇒ Boolean): IO[Unit] = {

    //Minimum number of threads is 2
    lazy val ex =
      java.util.concurrent.Executors.newFixedThreadPool(proc max 2)
    lazy val s = scalaz.concurrent.Strategy.Executor(ex)

    //flag to be set to true if reactive system should be shutdown
    var doKill = false

    //blocks the calling thread
    val cdl = new java.util.concurrent.CountDownLatch(1)

    for {
      r ← IO(new Reactor(step, () ⇒ doKill, cdl, s))
      _ ← in.syncTo { stop(_) ? IO{doKill = true} | IO.ioUnit }
            .run(Const(Never), r)
      _ ← IO(r.start())
      _ ← IO { cdl.await(); ex.shutdown() }
    } yield ()
  }
}

trait RFInstances {
  implicit val SFArrow: Arrow[SF] = new RFArrow[Id]

  implicit val EFArrow: Arrow[EF] = new RFArrow[Event]

  implicit def SFApplicative[R]
    : Applicative[({type λ[α]=RF[R,α,Id,Id]})#λ] =
    new RFApplicative[R,Id,Id]

  implicit def EFApplicative[R]
    : Applicative[({type λ[α]=RF[R,α,Event,Event]})#λ] =
    new RFApplicative[R,Event,Event]

  implicit def SEFApplicative[R]
    : Applicative[({type λ[α]=RF[R,α,Id,Event]})#λ] =
    new RFApplicative[R,Id,Event]

  implicit def ESFApplicative[R]
    : Applicative[({type λ[α]=RF[R,α,Event,Id]})#λ] =
    new RFApplicative[R,Event,Id]

  implicit def EFPlus[R]: PlusEmpty[({type λ[α]=EF[R,α]})#λ] =
    new RFPlus[R,Event]

  implicit def SEFPlus[R]: PlusEmpty[({type λ[α]=SEF[R,α]})#λ] =
    new RFPlus[R,Id]

  implicit def EFMonoid[A,B]: Monoid[EF[A,B]] = EFPlus[A].monoid

  implicit def SEFMonoid[A,B]: Monoid[SEF[A,B]] = SEFPlus[A].monoid
}

private[dire] sealed abstract class IdOrEvent[F[+_]](m: Applicative[F])
    extends Applicative[F] {
  def toEvent[A](f: F[A]): Event[A]
  def isNever[A](f: F[A]) = toEvent(f) fold (_ ⇒ false, true)
  def out[A](o: Out[A]): Out[F[A]] = toEvent(_) fold (o, IO.ioUnit)
  final def point[A](a: ⇒ A) = m point a
  final def ap[A,B](fa: ⇒ F[A])(f: ⇒ F[A ⇒ B]) = m.ap(fa)(f)
}

private[dire] object IdOrEvent {
  def apply[F[+_]: IdOrEvent]: IdOrEvent[F] = implicitly

  implicit val IdIdOrEvent: IdOrEvent[Id] = new IdOrEvent[Id](Applicative[Id]) {
    def toEvent[A](f: Id[A]): Event[A] = Once(f)
  }

  implicit val EventIdOrEvent: IdOrEvent[Event] =
    new IdOrEvent[Event](Event.EventMonad) {
      def toEvent[A](f: Event[A]) = f
    }
}

private[dire] class RFApplicative[R,I[+_]:IdOrEvent,O[+_]:IdOrEvent]
  extends Applicative[({type λ[α]=RF[R,α,I,O]})#λ] {
  def point[A](a: ⇒ A): RF[R,A,I,O] =
    RF.constRF[R,A,I,O](IdOrEvent[O] point a)

  def ap[A,B](fa: ⇒ RF[R,A,I,O])(f: ⇒ RF[R,A ⇒ B,I,O]) = fa ap f

  override def map[A,B](s: RF[R,A,I,O])(f: A ⇒ B) = s map f
}

private[dire] class RFPlus[R,I[+_]:IdOrEvent]
   extends PlusEmpty[({type λ[α]=RF[R,α,I,Event]})#λ] {
  def empty[A] = RF.constRF[R,A,I,Event](Never)
  def plus[A](a: RF[R,A,I,Event], b: ⇒ RF[R,A,I,Event]) = a merge b
}

private[dire] class RFArrow[I[+_]:IdOrEvent]
  extends Arrow[({type λ[α,b]=RF[α,b,I,I]})#λ] {
   def id[A]: RF[A,A,I,I] = RF.id

  def arr[A,B](f: A ⇒ B): RF[A,B,I,I] = id[A] map f

  def compose[A,B,C](f: RF[B,C,I,I], g: RF[A,B,I,I]) = f compose g

  def first[A,B,C](f: RF[A,B,I,I]): RF[(A,C),(B,C),I,I] = {
    val sfAC = id[(A,C)]
    val sfACB = sfAC map { _._1 } andThen f
    val sfACC = sfAC map { _._2 }

    (sfACB <*> sfACC){ Tuple2.apply }
  }
}

// vim: set ts=2 sw=2 nowrap et:
