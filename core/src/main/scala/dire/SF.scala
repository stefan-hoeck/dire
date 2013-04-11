package dire

import dire.control.{RawSignal ⇒ RS, Reactor}
import scala.reflect.runtime.universe.TypeTag
import scalaz._, Scalaz._, effect.IO

/** Represents a transformation from an input signal or 
  * event stream to an output signal or event stream.
  *
  * This class is the main entry point into reactive programming
  * with dire. It offers a rich set of combinators needed to describe
  * reactive networks.
  *
  * @tparam A  Type of events fired by the input signal or event stream
  * @tparam B  Type of events fired by the output signal or event stream
  */
class SF[-A,+B] private[dire](
    private[dire] val run: (RS[A], Reactor) ⇒ IO[RS[B]]) {
  import SF._ //, DataSink._

  /** Applies a time-changing function to the signals values */
  def ap[C,AA<:A](f: SF[AA,B ⇒ C]): SF[AA,C] = (f <*> this)(_ apply _)

  /** Sequentially combines two signal functions */
  def andThen[C](that: SF[B,C]): SF[A,C] = that compose this

//  /** Like `syncTo` but performs side effects asynchronuously,
//    * that is, the main `Reactor` thread is not blocked.
//    *
//    * The reactive system guarantees that, as long as the given
//    * `out` is not used as a data sink for other signals, it will
//    * only be accessed sequentially from this signal, i.e. it
//    * need not be thread safe. It is for instance possible to
//    * write all values of this signal to a file without being
//    * afraid of race conditions or other ugliness. If the same
//    * file is used to monitor several distinct signals or
//    * event streams they should either be merged in a single
//    * signal (event stream) first, or function `to` should
//    * be used which guarantees that all access from a given
//    * reactive graph will be sequential.
//    */
//  def asyncTo(out: Out[B]): SF[A,B,I,O] = to(DataSink async out)

  /** Connect a reactive branch to this signal function but
    * return to the original branch afterwards.
    */
  def branch[C](that: SF[B,C]): SF[A,B] = 
    SF { (ra,r) ⇒ run(ra, r) >>= { rb ⇒ that.run(rb,r) as rb } }

  /** Map and filter an event stream in one run */
  def collect[C](f: B ⇒ Option[C]): SF[A,C] =
    sync1(this)(_ collect f)((ceb,_) ⇒ ceb collect f)

  /** Sequentially combines two signal functions */
  def compose[C](that: SF[C,A]): SF[C,B] =
    SF { (rc, r) ⇒ that.run(rc,r) >>= { run(_, r) } }

  /** Contravariant mapping */
  def contramap[C](f: C ⇒ A): SF[C,B] = compose(SF.id[C] map f)

  /** Counts the number of events this event stream fired and
    * stores the results in a signal
    */
  def count: SF[A,Int] = scanMap { _ ⇒ 1 }

  /** Returns a signal that only fires an event if its new
    * value is different from its old one
    */
  def distinct[BB>:B:Equal]: SF[A,BB] =
    sync1[A,B,BB](this)(identity)(
      (eb,ec) ⇒ if (ec.toOption ≟ eb.toOption) Never else eb)

  /** Creates an event stream that fires whenever this signal
    * fires an event but skips the signal's initial value if any.
    */
  def events: SF[A,B] = sync1[A,B,B](this)(_ ⇒ Never)((cb,_) ⇒ cb)

  /** Filters an event stream according to the given predicate */
  def filter(p: B ⇒ Boolean): SF[A,B] = collect[B] { b ⇒ p(b) option b }

  /** Converts this event stream to a signal with initial value
    * `ini`
    *
    * If the original event stream fires an event at T0 its value is replaced
    * by `ini`.
    */
  def hold[C>:B](ini: C): SF[A,C] = scan[C](ini)((next,_) ⇒ next)

  /** Functor map */
  def map[C](f: B ⇒ C): SF[A,C] = SF { (ra,r) ⇒ run(ra,r) >>= { _ map f } }

  /** Returns an event stream that fires whenever one of the input streams
    * fire.
    *
    * If both event streams fire at the same time, the event of the
    * second (right) stream is ignored
    */
  def merge[AA<:A,BB>:B](that: SF[AA,BB]): SF[AA,BB] = {
    def later(c1: Event[BB], c2: Event[BB]) = (c1, c2) match {
      case (a@Once(at1,x), b@Once(at2,y)) ⇒ if (at1 >= at2) a else b
      case (ox, oy)                       ⇒ ox orElse oy
    }

    sync2(this,that)(later)((ca,cb,_) ⇒ later(ca,cb))
  }

  /** Returns an event stream that fires this signals actual value
    * whenever the given event stream fires
    */
  def on[C,AA<:A](ef: SF[AA,C]): SF[AA,B] = upon(ef)((b,_) ⇒ b)

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
  def scan[C](ini: ⇒ C)(next: (B,C) ⇒ C): SF[A,C] = {
      def in(cb: Event[B]) = cb map { next(_,ini) } orElse Once(T0, ini)

      sync1(this)(in)(^(_,_)(next))
    }

  /** Accumulates events by first transferring them to a value
    * with a Monoid instance
    */
  def scanMap[C:Monoid](f: B ⇒ C): SF[A,C] = scan(∅[C])((a,b) ⇒ b ⊹ f(a))

  /** Accumulates events in a container */
  def scanPlus[F[+_]](implicit P: ApplicativePlus[F])
    : SF[A,F[B]] = scanMap(_.η[F])(P.monoid)

//  /** Accumulates the results of a stateful calculation */
//  def scanSt[S,C](ini: S)(implicit WS: B <:< State[S,C], W: AsEv[O])
//    : SF[A,(S,C),I,Event] = {
//    def sfE = scan[Event[(S,C)]](Never)((s: B, e: Event[(S,C)]) ⇒ 
//      e map { s run _._1 } orElse Once(s run ini))
//
//    SF[A,(S,C),I,Event](sfE.run)
//  }
//
//  /** Accumulates the results of a stateful calculation in a signal
//    * starting at value `ini`.
//    *
//    * Note that the stateful calculation is performed purely for its
//    * effects on the state and the result is discarded.
//    */
//  def scanStHold[S,C]
//    (ini: S)
//    (implicit WS: B <:< State[S,C], W: AsEv[O]): SF[A,S,I,Id] =
//    scanStS[S,C](ini) hold ini
//
//  /** Accumulates the results of a stateful calculation 
//    * keeping the state and discarding the result
//    */
//  def scanStS[S,C]
//    (ini: S)
//    (implicit WS: B <:< State[S,C], W: AsEv[O]): SF[A,S,I,Event] =
//    scanSt[S,C](ini) map { _._1 }
//
//  /** Accumulates the results of a stateful calculation 
//    * discarding the new state
//    */
//  def scanStV[S,C](ini: S)
//    (implicit WS: B <:< State[S,C], W: AsEv[O]): SF[A,C,I,Event] =
//    scanSt[S,C](ini) map { _._2 }

  /** Accumulates events using a Monoid */
  def sum[BB>:B](implicit M: Monoid[BB]): SF[A,BB] = scanMap[BB](identity)

//  /** Performs the given IO-action with this signal's initial value
//    * and whenever this signal changes.
//    *
//    * Note that `out` will be called by the `Reactor`'s
//    * running thread that updates
//    * the main reactive graph. Therefore, side effects performed
//    * by `out` need to be fast if the system should stay reactive.
//    * Consider using `asyncTo` instead for fully asynchronuous
//    * side effects or when side effects have to be performed in a
//    * special type of thread (the Swing event dispatch thread for instance).
//    */
//  def syncTo(out: Out[B]): SF[A,B,I,O] = to(DataSink sync out)
//
//  /** Asynchronuously output the values of this signal to a data sink
//    *
//    * How the data sink operates and what concurrency strategy is
//    * applied is defined in the [[dire.DataSink]] type class.
//    */
//  def to(sink: DataSink[B]): SF[A,B,I,O] = 
//    SF((ra,r) ⇒ run(ra,r) >>= { rb ⇒ sink.connect[O](rb,r) as rb })

  /** Combines a signal with an event stream through a
    * function of arity two.
    *
    * The resulting event stream fires only, when the given
    * event stream fires.
    */
  def upon[C,D,AA<:A](ef: SF[AA,C])
                     (f: (B,C) ⇒ D): SF[AA,D] = {
    def g(eb: Event[B], ec: Event[C]): Event[D] =
      if (ec.at >= eb.at) ^(eb,ec)(f) else Never

    sync2(this,ef)(g)((eb,ec,_) ⇒ g(eb,ec))
  }

  /** Alias for `andThen` */
  def >=>[C](that: SF[B,C]): SF[A,C] = andThen(that)

  /** Alias for `compose` */
  def <=<[C](that: SF[C,A]): SF[C,B] = compose(that)

  /** Combines two signals with a pure function */
  def <*>[C,D,AA<:A](that: SF[AA,C])(f: (B,C) ⇒ D): SF[AA,D] =
    sync2(this, that)(^(_,_)(f))((cb,cc,_) ⇒ ^(cb, cc)(f))
//
//  /** Alias for `contramap`*/
//  def ∙ [C](f: C ⇒ A): SF[C,B,I,O] = contramap(f)
//
//  /** Alias for `asyncTo` */
//  def -/->(out: Out[B]): SF[A,B,I,O] = asyncTo(out)
//
//  /** Alias for `syncTo` */
//  def -->(out: Out[B]): SF[A,B,I,O] = syncTo(out)
//
//  /** Alias for `to` */
//  def ==>(sink: DataSink[B]): SF[A,B,I,O] = to(sink)

  /** Appends a signal or event source to this signal function
    *
    * The resulting signal function will run both, the original
    * and the new signal function. This is useful to concatenate
    * completely unrelated sources
    */
  def >>[C](that: SIn[C]): SF[A,C] =
    SF { (ra,r) ⇒ run(ra,r) >> that.run(RS.empty,r) }
}

object SF extends SFFunctions with SFInstances {

  private[dire] def apply[A,B](run: (RS[A], Reactor) ⇒ IO[RS[B]]): SF[A,B] =
    new SF(run)

  private[dire] lazy val processors =
    Runtime.getRuntime.availableProcessors
}

trait SFFunctions {
  lazy val ms = 1000L

  lazy val s = ms * 1000L

  lazy val min = s * 60L

  lazy val h = min * 60L

  // ***                  *** //
  // *** Constant signals *** //
  // ***                  *** //

  /** A constant signal that never changes */
  def const[A](a: ⇒ A): SIn[A] = SF[⊥,A] { (_,_) ⇒ RS const a }

  /** The empty event stream that never fires an event */
  def never[A]: SIn[A] = SF[⊥,A] { (_,_) ⇒ RS.never }

  // ***                                            *** //
  // *** Basic signal and event stream transformers *** //
  // ***                                            *** //

  /** The identity signal function */
  def id[A]: SF[A,A] = SF { (ra,_) ⇒ IO(ra) }

  /** Signal function from a pure function */
  def sf[A,B](f: A ⇒ B): SF[A,B] = id map f
//  
//  /** Asynchronously runs the given IO action whenever the
//    * input event stream fires.
//    *
//    * The resulting event stream fires its own even, whenever
//    * the result of `f` is ready.
//    */
//  def efIO[A,B](f: A ⇒ IO[B]): EF[A,B] =
//    SF[A,B,Event,Event] { (ea,r) ⇒ r.trans(f)(ea) }
//
//  /** Asynchronuously loops back the output of the given
//    * event stream to its input
//    */
//  def loop[A](ef: EF[A,A]): EIn[A] = 
//    SF[A,A,Event,Event] { (ea,r) ⇒ r.loop[Event[A]](Never)(ef.run) }

  // ***                   *** //
  // *** Sources and Sinks *** //
  // ***                   *** //

  /** Creates a source signal from an external data source
    *
    * Unlike `src`, the resulting signal function is cached using `s`
    * as a key.
    */
  def cachedSrc[S,V:TypeTag](s: S)(implicit Src: DataSource[S,V])
    : SIn[V] = cached[⊥,V](src(s), s)

  /** Creates a source signal from an external data source
    *
    * See the [[dire.DataSource]] type class for more details
    */
  def src[S,V](s: S)(implicit Src: DataSource[S,V]): SIn[V] =
    SF[⊥,V] { (_,r) ⇒ r.source(Src ini s)(Src cb s) }

  /** Asynchronuously fires the given event once */
  def once[A](a: ⇒ A): SIn[A] = src(())(DataSource once a)


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
  def cached[A:TypeTag,B:TypeTag](sf: SF[A,B], tag: Any): SF[A,B] =
    SF[A,B] { (ra,r) ⇒ r.cached(sf.run, tag)(ra) }


  // ***              *** //
  // *** Time signals *** //
  // ***              *** //

  /** The time signal
    *
    * In every isolated reactive system there is only one such signal
    */
  def time: SIn[Time] = SF[⊥,Time] { (_,r) ⇒ r.timeSignal }

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
  def ticks(step: Time): SIn[Unit] = src[Time,Unit](step)
//
//
//  // ***                          *** //
//  // *** Running signal functions *** //
//  // ***                          *** //
//
//  /** Sets up a reactive network and runs it until the given
//    * abort condition is fullfilled.
//    *
//    * Note that the calling thread is blocked until the abbort
//    * condition is fullfilled and the reactive system shutdown
//    *
//    * @param in  The signal function that describes the reactive
//    *            network
//    *
//    * @param step  Time resolution in microseconds. If the given
//    *              signal function depends on the `Time` signal
//    *              this number denotes how frequently `Time` is
//    *              being updated. The default value is `1000L`
//    *              (one millisecond)
//    *
//    * @param proc Number of processors (threads) available to the
//    *             actors running in the background. The default
//    *             value is the total number of processors of the
//    *             system the application is running on. Note that
//    *             a minimum of two threads is chosen if `proc` is
//    *             less than two.
//    *
//    * @param stop This function should return `true` when a certain
//    *             abbort condition is fulfilled. In that case, the
//    *             reactive framework will cease to run and release
//    *             all its resources. Note that the framework will
//    *             stop immediately AFTER `stop` has returned true
//    *             but that the event that lead to abortion
//    *             is still processed and passed to all
//    *             registered data sinks.
//    */
//  def runS[A]
//    (in: SIn[A], proc: Int = SF.processors, step: Time = 1000L)
//    (stop: A ⇒ Boolean)
//    : IO[Unit] =
//    run(in, proc, step)(stop)
//
//  /** Sets up a reactive network and runs it until the given
//    * abort condition is fullfilled.
//    *
//    * Note that the calling thread is blocked until the abbort
//    * condition is fullfilled and the reactive system shutdown
//    *
//    * @param in  The event source that describes the reactive
//    *            network
//    *
//    * @param step  Time resolution in microseconds. If the given
//    *              signal function depends on the `Time` signal
//    *              this number denotes how frequently `Time` is
//    *              being updated. The default value is `1000L`
//    *              (one millisecond)
//    *
//    * @param proc Number of processors (threads) available to the
//    *             actors running in the background. The default
//    *             value is the total number of processors of the
//    *             system the application is running on. Note that
//    *             a minimum of two threads is chosen if `proc` is
//    *             less than two.
//    *
//    * @param stop This function should return `true` when a certain
//    *             abbort condition is fulfilled. In that case, the
//    *             reactive framework will cease to run and release
//    *             all its resources. Note that the framework will
//    *             stop immediately AFTER `stop` has returned true
//    *             but that the event that lead to abortion
//    *             is still processed and passed to all
//    *             registered data sinks.
//    */
//  def runE[A]
//    (in: EIn[A], proc: Int = SF.processors, step: Time = 1000L)
//    (stop: A ⇒ Boolean)
//    : IO[Unit] =
//    run[A,Event](in, proc, step)(stop)
//
//
//  // ***                                   *** //
//  // ***  package private helper functions *** //
//  // ***                                   *** //
//
//  private[dire] def id[A,F[+_]:IdOrEvent]: SF[A,A,F,F] = SF((a,_) ⇒ IO(a))

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
  private[dire] def sync2[R,A,B,C]
    (sa: SF[R,A], sb: SF[R,B])
    (ini: (Event[A], Event[B]) ⇒ Event[C])
    (next: (Event[A], Event[B], Event[C]) ⇒ Event[C])
    : SF[R,C] = 
    sync2O(sa, sb)(ini(_,_).toOption)(next(_,_,_).toOption)

  private[dire] def sync2O[R,A,B,C]
    (sa: SF[R,A], sb: SF[R,B])
    (ini: (Event[A], Event[B]) ⇒ Option[C])
    (next: (Event[A], Event[B], Event[C]) ⇒ Option[C]): SF[R,C] =
    SF[R,C]((rr, r) ⇒ 
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
  private[dire] def sync1[R,A,B]
    (sa: SF[R,A])
    (ini: Event[A] ⇒ Event[B])
    (next: (Event[A], Event[B]) ⇒ Event[B])
    : SF[R,B] = sync1O(sa)(ini(_).toOption)(next(_,_).toOption)

  private[dire] def sync1O[R,A,B]
    (sa: SF[R,A])
    (ini: Event[A] ⇒ Option[B])
    (next: (Event[A], Event[B]) ⇒ Option[B]): SF[R,B] =
    SF[R,B]((rr, r) ⇒ 
      for {
        ra ← sa.run(rr, r)
        rb ← RS.sync1(ra)(ini)(next)
      } yield rb
    )

//  private[dire] def run[A]
//    (in: SIn[A], proc: Int, step: Time)
//    (stop: A ⇒ Boolean): IO[Unit] = {
//
//    //Minimum number of threads is 2
//    lazy val ex =
//      java.util.concurrent.Executors.newFixedThreadPool(proc max 2)
//    lazy val s = scalaz.concurrent.Strategy.Executor(ex)
//
//    //flag to be set to true if reactive system should be shutdown
//    var doKill = false
//
//    //blocks the calling thread
//    val cdl = new java.util.concurrent.CountDownLatch(1)
//
//    for {
//      r ← IO(new Reactor(step, () ⇒ doKill, cdl, s))
//      _ ← in.syncTo { stop(_) ? IO{doKill = true} | IO.ioUnit }
//            .run(RS.empty, r)
//      _ ← IO(r.start())
//      _ ← IO { cdl.await(); ex.shutdown() }
//    } yield ()
//  }
}

trait SFInstances {
//  implicit val SFArrow: Arrow[SF] = new SFArrow[Id]
//
//  implicit val EFArrow: Arrow[EF] = new SFArrow[Event]
//
//  implicit def SFApplicative[R]
//    : Applicative[({type λ[α]=SF[R,α,Id,Id]})#λ] =
//    new SFApplicative[R,Id,Id]
//
//  implicit def EFApplicative[R]
//    : Applicative[({type λ[α]=SF[R,α,Event,Event]})#λ] =
//    new SFApplicative[R,Event,Event]
//
//  implicit def SEFApplicative[R]
//    : Applicative[({type λ[α]=SF[R,α,Id,Event]})#λ] =
//    new SFApplicative[R,Id,Event]
//
//  implicit def ESFApplicative[R]
//    : Applicative[({type λ[α]=SF[R,α,Event,Id]})#λ] =
//    new SFApplicative[R,Event,Id]
//
//  implicit def EFPlus[R]: PlusEmpty[({type λ[α]=EF[R,α]})#λ] =
//    new SFPlus[R,Event]
//
//  implicit def SEFPlus[R]: PlusEmpty[({type λ[α]=SEF[R,α]})#λ] =
//    new SFPlus[R,Id]
//
//  implicit def EFMonoid[A,B]: Monoid[EF[A,B]] = EFPlus[A].monoid
//
//  implicit def SEFMonoid[A,B]: Monoid[SEF[A,B]] = SEFPlus[A].monoid
}

//private[dire] class SFApplicative[R,I[+_]:IdOrEvent,O[+_]:IdOrEvent]
//  extends Applicative[({type λ[α]=SF[R,α,I,O]})#λ] {
//  def point[A](a: ⇒ A): SF[R,A,I,O] =
//    SF.constSF[R,A,I,O](IdOrEvent[O] point a)
//
//  def ap[A,B](fa: ⇒ SF[R,A,I,O])(f: ⇒ SF[R,A ⇒ B,I,O]) = fa ap f
//
//  override def map[A,B](s: SF[R,A,I,O])(f: A ⇒ B) = s map f
//}
//
//private[dire] class SFPlus[R,I[+_]:IdOrEvent]
//   extends PlusEmpty[({type λ[α]=SF[R,α,I,Event]})#λ] {
//  def empty[A] = SF.constSF[R,A,I,Event](Never)
//  def plus[A](a: SF[R,A,I,Event], b: ⇒ SF[R,A,I,Event]) = a merge b
//}
//
//private[dire] class SFArrow[I[+_]:IdOrEvent]
//  extends Arrow[({type λ[α,b]=SF[α,b,I,I]})#λ] {
//   def id[A]: SF[A,A,I,I] = SF.id
//
//  def arr[A,B](f: A ⇒ B): SF[A,B,I,I] = id[A] map f
//
//  def compose[A,B,C](f: SF[B,C,I,I], g: SF[A,B,I,I]) = f compose g
//
//  def first[A,B,C](f: SF[A,B,I,I]): SF[(A,C),(B,C),I,I] = {
//    val sfAC = id[(A,C)]
//    val sfACB = sfAC map { _._1 } andThen f
//    val sfACC = sfAC map { _._2 }
//
//    (sfACB <*> sfACC){ Tuple2.apply }
//  }
//}

// vim: set ts=2 sw=2 nowrap et:
