package dire

import dire.control.{RawSignal ⇒ RS, Reactor}
import java.util.concurrent.{ExecutorService, Executors}
import scala.reflect.runtime.universe.TypeTag
import scala.concurrent.{Future, Await, duration}, duration.Duration.Inf
import scalaz.Leibniz.===
import scalaz._, Scalaz._, effect.IO, Liskov.<~<
import scalaz.concurrent.Strategy

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
class SF[A,B] private[dire](
    private[dire] val run: (RS[A], Reactor) ⇒ IO[RS[B]]) {
  import SF._

  /** Applies a time-changing function to the signals values */
  def ap[C](f: SF[A,B ⇒ C]): SF[A,C] = (f <*> this)(_ apply _)

  /** Sequentially combines two signal functions */
  def andThen[C](that: SF[B,C]): SF[A,C] = that compose this

  /** Like `syncTo` but performs side effects asynchronously,
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
  def asyncTo(out: Out[B]): SF[A,B] = to(DataSink async out)

  /** Connect a reactive branch to this signal function but
    * return to the original branch afterwards.
    */
  def branch[C](that: SF[B,C]): SF[A,B] = 
    SF { (ra,r) ⇒ run(ra, r) >>= { rb ⇒ that.run(rb,r) as rb } }

  /** Map and filter an event stream in one run */
  def collect[C](f: PartialFunction[B,C]): SF[A,C] =
    collectO(f.lift)

  /** Collects failed `Validation`s only */
  def collectF[C,D](implicit W: B <:< Validation[C,D]): SF[A,C] =
    collectO { _.swap.toOption }

  /** Collects left disjunctions only */
  def collectL[C,D](implicit W: B <:< (C \/ D)): SF[A,C] =
    collectO { _.swap.toOption }

  /** Map and filter an event stream in one run */
  def collectO[C](f: B ⇒ Option[C]): SF[A,C] =
    sync1(this)(_ collect f)((ceb,_) ⇒ ceb collect f)

  /** Collects right disjunctions only */
  def collectR[C,D](implicit W: B <:< (C \/ D)): SF[A,D] =
    collectO { _.toOption }

  /** Collects successful `Validation`s only */
  def collectS[C,D](implicit W: B <:< Validation[C,D]): SF[A,D] =
    collectO { _.toOption }

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
  def distinct(implicit B: Equal[B]): SF[A,B] =
    sync1[A,B,B](this)(identity)(
      (eb,ec) ⇒ if (ec.toOption ≟ eb.toOption) Event.never else eb)

  /** Ignores the first n events and passes on the rest */
  def drop(n: Int): SF[A,B] = {
    def st(b: B) = State { i: Int ⇒ (i - 1, i <= 0 option b) }

    map(st) scanStV n collectO identity
  }

  /** Creates an event stream that fires whenever this signal
    * fires an event but skips the signal's initial value if any.
    */
  def events: SF[A,B] = sync1[A,B,B](this)(_ ⇒ Event.never)((cb,_) ⇒ cb)

  // used in unit tests
  private[dire] def eventsTo(out: Out[Event[B]]): SF[A,B] =
    to(DataSink syncE out)

  /** Filters an event stream according to the given predicate */
  def filter(p: B ⇒ Boolean): SF[A,B] = collectO[B] { b ⇒ p(b) option b }

  /** Filters an event stream according to the given predicate */
  def filterNot(p: B ⇒ Boolean): SF[A,B] = filter { b ⇒ ! p(b) }

  /** Accumulates successive events in lists of size `n` */
  def grouped(n: Int): SF[A,List[B]] = {
    def st(b: B) = State { p: (Int,List[B]) ⇒ p match {
        case (i,bs) if i <= 0 ⇒ ((n - 1, List(b)), bs.reverse.some)
        case (i,bs)           ⇒ ((i - 1, b :: bs), none)
      }
    }

    if (n <= 0) >>(SF.never)
    else map(st) scanStV (n, List.empty[B]) collectO identity
  }

  /** Accumulates successive events in pairs */
  def groupAsPairs: SF[A,(B,B)] =
    grouped(2) collect { case a::b::Nil ⇒ (a,b) }

  /** Accumulates successive events in triples */
  def groupAsTriples: SF[A,(B,B,B)] =
    grouped(3) collect { case a::b::c::Nil ⇒ (a,b,c) }

  /** Passes on the first event and ignores the rest */
  def head: SF[A,B] = take(1)

  /** Converts this event stream to a signal with initial value
    * `ini`
    *
    * If the original event stream fires an event at T0 its value is replaced
    * by `ini`.
    */
  def hold(ini: ⇒ B): SF[A,B] = 
    sync1(this)(e ⇒ Event.once(e.at, ini))((e,_) ⇒ e)

  /** Transforms this signal function into an input signal function
    * (`SIn`) by prepending the empty event stream.
    *
    * This of course means, that unless the original signal function
    * results in a behavior that fires events independently of its
    * input behavior, the resulting resulting behavior of the input
    * signal will never fire an event.
    */
  def in: SIn[B] = SF.never >=> this

  /** Functor map */
  def map[C](f: B ⇒ C): SF[A,C] = SF { (ra,r) ⇒ run(ra,r) >>= { _ map f } }

  /** Returns an event stream that fires whenever one of the input streams
    * fire.
    *
    * If both event streams fire at the same time, the event of the
    * second (right) stream is ignored
    */
  def merge(that: SF[A,B]): SF[A,B] = {
    def later(c1: Event[B], c2: Event[B]) = (c1, c2) match {
      case (a@Once(at1,x), b@Once(at2,y)) ⇒ if (at1 >= at2) a else b
      case (ox, oy)                       ⇒ ox orElse oy
    }

    sync2(this,that)(later)((ca,cb,_) ⇒ later(ca,cb))
  }

  /** Returns an event stream that fires this signals actual value
    * whenever the given event stream fires
    */
  def on[C](ef: SF[A,C]): SF[A,B] = upon(ef)((b,_) ⇒ b)

  /** Merges two event streams returning an event stream of a disjunction */
  def or[C](sf: SF[A,C]): SF[A,B \/ C] =
    map{ _.left[C] } merge sf.map{ _.right[B] }

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
  def scanPlus[F[_]](implicit P: ApplicativePlus[F]): SF[A,F[B]] =
    scanMap(_.η[F])(P.monoid)

  /** Accumulates the results of a stateful calculation */
  def scanSt[S,C](ini: S)(implicit WS: B <~< State[S,C]): SF[A,(S,C)] = {
    def iniE(e: Event[B]) = e map { WS(_) run ini }
    def next(e: Event[B], s: Event[(S,C)]) =
      e map { WS(_) run s.fold(_._1, ini) }

    sync1(this)(iniE)(next)
  }

  /** Accumulates the results of a stateful calculation 
    * keeping the state and discarding the result
    */
  def scanStS[S,C](ini: S)(implicit WS: B <~< State[S,C]): SF[A,S] =
    scanSt[S,C](ini) map { _._1 }

  /** Accumulates the results of a stateful calculation 
    * discarding the new state
    */
  def scanStV[S,C](ini: S)(implicit WS: B <~< State[S,C]): SF[A,C] =
    scanSt[S,C](ini) map { _._2 }

  /** Transforms this input signal function into a signal function
    * that transforms behaviors of type `C`.
    *
    * This can be useful when combining input signal functions with
    * other signal transformers.
    */
  def sf[C](implicit WS: A === In): SF[C,B] = 
    SF.id[C] >> sin

  /** Changes the type of this signal function to `SIn`.
    *
    * This can be useful to improve type inference.
    */
  def sin(implicit WS: A === In): SIn[B] =
    WS.subst[({type λ[α]=SF[α,B]})#λ](this)

  /** Accumulates successive events in slides of size `n` */
  def sliding(n: Int): SF[A,List[B]] = {
    def st(b: B) = State { p: (Int,List[B]) ⇒ p match {
        case (i,bs) if i <= 1 ⇒ {
          val newBs = b :: bs
          ((i, newBs.init), newBs.reverse.some)
        }
        case (i,bs)           ⇒ ((i - 1, b :: bs), none)
      }
    }

    if (n <= 0) >>(SF.never)
    else map(st) scanStV (n, List.empty[B]) collectO identity
  }

  /** Accumulates successive events in pairs */
  def slidingAsPairs: SF[A,(B,B)] =
    sliding(2) collect { case a::b::Nil ⇒ (a,b) }

  /** Accumulates successive events in triples */
  def slidingAsTriples: SF[A,(B,B,B)] =
    sliding(3) collect { case a::b::c::Nil ⇒ (a,b,c) }

  /** Accumulates events using a Monoid */
  def sum(implicit M: Monoid[B]): SF[A,B] = scanMap[B](identity)

  /** Performs the given IO-action with this signal's initial value
    * and whenever this signal changes.
    *
    * Note that `out` will be called by the `Reactor`'s
    * running thread that updates
    * the main reactive graph. Therefore, side effects performed
    * by `out` need to be fast if the system should stay reactive.
    * Consider using `asyncTo` instead for fully asynchronous
    * side effects or when side effects have to be performed in a
    * special type of thread (the Swing event dispatch thread for instance).
    */
  def syncTo(out: Out[B]): SF[A,B] = to(DataSink sync out)

  /** Drops the first event and passes on the rest */
  def tail: SF[A,B] = drop(1)

  /** Passes on the first n events and ignores the rest */
  def take(n: Int): SF[A,B] = {
    def st(b: B) = State { i: Int ⇒ (i - 1, i > 0 option b) }

    map(st) scanStV n collectO identity
  }

  /** Asynchronously output the values of this signal to a data sink
    *
    * How the data sink operates and what concurrency strategy is
    * applied is defined in the [[dire.DataSink]] type class.
    */
  def to(sink: DataSink[B]): SF[A,B] = 
    SF { (ra,r) ⇒ run(ra,r) >>= { rb ⇒ sink.connect(rb,r) as rb } }

  /** Combines a signal with an event stream through a
    * function of arity two.
    *
    * The resulting event stream fires only, when the given
    * event stream fires.
    */
  def upon[C,D](ef: SF[A,C])(f: (B,C) ⇒ D): SF[A,D] = {
    def g(eb: Event[B], ec: Event[C]): Event[D] =
      if (ec.at >= eb.at) ^(eb,ec)(f) else Event.never

    sync2(this,ef)(g)((eb,ec,_) ⇒ g(eb,ec))
  }

  /** Zips together the events of two behaviors */
  def zip[C](that: SF[A,C]): SF[A,(B,C)] = <*>(that)(Tuple2.apply)

  /** Alias for `andThen` */
  def >=>[C](that: SF[B,C]): SF[A,C] = andThen(that)

  /** Alias for `compose` */
  def <=<[C](that: SF[C,A]): SF[C,B] = compose(that)

  /** Combines two signals with a pure function */
  def <*>[C,D](that: SF[A,C])(f: (B,C) ⇒ D): SF[A,D] =
    sync2(this, that)(^(_,_)(f))((cb,cc,_) ⇒ ^(cb, cc)(f))

  /** Alias for `contramap`*/
  def ∙ [C](f: C ⇒ A): SF[C,B] = contramap(f)

  /** Alias for `asyncTo` */
  def -/->(out: Out[B]): SF[A,B] = asyncTo(out)

  /** Alias for `syncTo` */
  def -->(out: Out[B]): SF[A,B] = syncTo(out)

  /** Alias for `to` */
  def ==>(sink: DataSink[B]): SF[A,B] = to(sink)

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
  def const[A](a: ⇒ A): SIn[A] = SF { (_,_) ⇒ RS const a }

  /** The empty event stream that never fires an event */
  def never[A]: SIn[A] = SF { (_,_) ⇒ RS.never }

  // ***                                            *** //
  // *** Basic signal and event stream transformers *** //
  // ***                                            *** //

  /** The identity signal function */
  def id[A]: SF[A,A] = SF { (ra,_) ⇒ IO(ra) }

  /** A signal function might depend on some external state
    * so it can only be created within the `IO`-Monad. This
    * functions transforms such a signal function into a
    * pure one. This can be useful in unit tests as well as
    * when creating stateful objects alongside the signal function
    * that do not have to be exposed to the outside world.
    */
  def io[A,B](sf: IO[SF[A,B]]): SF[A,B] = 
    SF { (ra,r) ⇒ sf >>= { _ run (ra, r) } }

  /** Signal function from a pure function */
  def sf[A,B](f: A ⇒ B): SF[A,B] = id map f
  
  private def sfIO[A,B](f: A ⇒ IO[B], s: StrategyO): SF[A,B] =
    SF { (ra,r) ⇒ r.trans(f, s)(ra) }

  /** Synchronously runs the given IO action whenever the
    * input event stream fires.
    *
    * Running the IO action blocks the main reactor thread, therefore it
    * should be reasonably fast compared to other events fired in the same
    * reactor.
    */
  def syncIO[A,B](f: A ⇒ IO[B]): SF[A,B] = sfIO(f, SSS)

  /** Asynchronously runs the given IO action whenever the
    * input event stream fires.
    *
    * The resulting event stream fires its own event, whenever
    * the result of `f` is ready.
    */
  def asyncIO[A,B](f: A ⇒ IO[B]): SF[A,B] = sfIO(f, None)

  def connectOuts[A,B](f: Out[B] ⇒ Out[A], s: StrategyO)
    : SF[A,B] = SF { (ra,r) ⇒ r.connectOuts(f, s)(ra) }

  //@TODO: Documentation
  def connectSync[A,B](f: Out[B] ⇒ Out[A]): SF[A,B] = connectOuts(f, SSS)

  //@TODO: Documentation
  def connectAsync[A,B](f: Out[B] ⇒ Out[A]): SF[A,B] = connectOuts(f, None)

  /** Asynchronously loops back the output of the given
    * event stream to its input
    */
  def loop[A](sf: SF[A,A]): SF[A,A] = SF { (ra,r) ⇒ r.loop(sf.run)(ra) }

  /** Like the function defined on class `SF` */
  def drop[A](n: Int): SF[A,A] = id drop n

  /** Like the function defined on class `SF` */
  def head[A]: SF[A,A] = id.head

  /** Like the function defined on class `SF` */
  def take[A](n: Int): SF[A,A] = id take n

  /** Like the function defined on class `SF` */
  def tail[A]: SF[A,A] = id.tail

  // ***                   *** //
  // *** Sources and Sinks *** //
  // ***                   *** //

  /** Creates a source signal from an external data source
    *
    * Unlike `src`, the resulting signal function is cached using `s`
    * as a key.
    */
  def cachedSrc[S,V:TypeTag](s: S)(implicit Src: DataSource[S,V])
    : SIn[V] = cached[In,V](src(s), s)

  /** Creates a source signal from an external data source
    *
    * See the [[dire.DataSource]] type class for more details
    */
  def src[S,V](s: S)(implicit Src: DataSource[S,V]): SIn[V] =
    SF { (_,r) ⇒ r.source(Src ini s)(Src cb s) }

  /** Asynchronously fires the given event once */
  def once[A](a: ⇒ A): SIn[A] = all(List(a))

  /** Asynchronously fires each of the given events once */
  def all[A,F[_]:Foldable](as: ⇒ F[A]): SIn[A] =
    src(())(DataSource all as.toList)


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
  def time: SIn[Time] = SF { (_,r) ⇒ r.timeSignal }

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
    * Note that an arbitrary number of completely independent
    * event streams can thus be created. 
    */
  def ticks(step: Time): SIn[Unit] = src[Time,Unit](step)


  // ***                          *** //
  // *** Running signal functions *** //
  // ***                          *** //

  /** Sets up a reactive network and runs it until the given
    * abort condition is fulfilled.
    *
    * Note that the calling thread is blocked until the abort
    * condition is fulfilled and the reactive system shutdown
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
    *             abort condition is fulfilled. In that case, the
    *             reactive framework will cease to run and release
    *             all its resources. Note that the framework will
    *             stop immediately AFTER `stop` has returned true
    *             but that the event that lead to abortion
    *             is still processed and passed to all
    *             registered data sinks.
    */
  def run[A] (in: SIn[A], proc: Int = SF.processors, step: Time = 1000L)
             (stop: A ⇒ Boolean): IO[Unit] = {

    //Minimum number of threads is 2
    lazy val ex = Executors.newFixedThreadPool(proc max 2)
    lazy val s = Strategy.Executor(ex)

    runS(in, s, step)(stop) >> IO(ex.shutdown())
  }

  /** Sets up a reactive network and runs it until the given
    * abort condition is fulfilled.
    *
    * Note that the calling thread is blocked until the abort
    * condition is fulfilled and the reactive system shutdown.
    * Note also, that if the provided strategy for parallel
    * execution is single-threaded, some constructs like loops
    * may lead to non-termination. It is far better and safer
    * to use `run` instead.
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
    * @param strategy Strategy used for running the actor system
    *                 in the background. This must not be single
    *                 threaded if recursive constructs like loops
    *                 are used.
    *
    * @param stop This function should return `true` when a certain
    *             abort condition is fulfilled. In that case, the
    *             reactive framework will cease to run and release
    *             all its resources. Note that the framework will
    *             stop immediately AFTER `stop` has returned true
    *             but that the event that lead to abortion
    *             is still processed and passed to all
    *             registered data sinks.
    */
  def runS[A](in: SIn[A], strategy: ⇒ Strategy, step: Time = 1000L)
             (stop: A ⇒ Boolean): IO[Unit] = {
    //flag to be set to true if reactive system should be shutdown
    var doKill = false

    //blocks the calling thread
    val cdl = new java.util.concurrent.CountDownLatch(1)

    for {
      r ← IO(new Reactor(step, () ⇒ doKill, cdl, strategy))
      _ ← in.syncTo { stop(_) ? IO{ doKill = true } | IO.ioUnit }
            .run(RS.empty, r)
      _ ← IO { r.start(); cdl.await() }
    } yield ()
  }

  // ***                                   *** //
  // ***  package private helper functions *** //
  // ***                                   *** //

  /** Creates a derived signal depending on two input signals
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
    (next: (Event[A], Event[B], Event[C]) ⇒ Event[C]): SF[R,C] =
    SF[R,C]((rr, r) ⇒ 
      for {
        ra ← sa.run(rr, r)
        rb ← sb.run(rr, r)
        rc ← RS.sync2(ra, rb)(ini)(next)
      } yield rc
    )

  /** Creates a derived signal depending on one input signal
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
    (next: (Event[A], Event[B]) ⇒ Event[B]): SF[R,B] =
    SF[R,B]((rr, r) ⇒ 
      for {
        ra ← sa.run(rr, r)
        rb ← RS.sync1(ra)(ini)(next)
      } yield rb
    )
}

trait SFInstances {
  implicit def SFArrow: Arrow[SF] = SFArrowImpl

  implicit def SFApplicative[R]: Applicative[({type λ[α]=SF[R,α]})#λ] =
    new SFApplicativeImpl[R]

  implicit def SFPlus[R]: PlusEmpty[({type λ[α]=SF[R,α]})#λ] =
    new SFPlusImpl[R]

  implicit def SFMonoid[A,B]: Monoid[SF[A,B]] = SFPlus[A].monoid
}

private[dire] class SFApplicativeImpl[R]
   extends Applicative[({type λ[α]=SF[R,α]})#λ] {
  def point[A](a: ⇒ A): SF[R,A] = SF.id[R] >> SF.const(a)
  def ap[A,B](fa: ⇒ SF[R,A])(f: ⇒ SF[R,A ⇒ B]) = fa ap f
  override def map[A,B](s: SF[R,A])(f: A ⇒ B) = s map f
}

private[dire] class SFPlusImpl[R]
   extends PlusEmpty[({type λ[α]=SF[R,α]})#λ] {
  def empty[A] = SF.id[R] >> SF.never[A]
  def plus[A](a: SF[R,A], b: ⇒ SF[R,A]) = a merge b
}

private[dire] object SFArrowImpl extends Arrow[SF] with Choice[SF] {
  def id[A]: SF[A,A] = SF.id
  def arr[A,B](f: A ⇒ B): SF[A,B] = id[A] map f
  def compose[A,B,C](f: SF[B,C], g: SF[A,B]) = f compose g

  def first[A,B,C](f: SF[A,B]): SF[(A,C),(B,C)] =
    (arr { p: (A,C) ⇒ p._1 } andThen f) ⊛ 
    (arr { p: (A,C) ⇒ p._2 }) apply Tuple2.apply

  def choice[A,B,C](f: => SF[A,C], g: => SF[B,C]): SF[A \/ B,C] =
    (id[A\/B].collectL >=> f) ⊹ (id[A\/B].collectR >=> g)
}

// vim: set ts=2 sw=2 nowrap et:
