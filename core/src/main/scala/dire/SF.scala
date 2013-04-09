package dire

//import dire.control.{RawSignal ⇒ RS, Reactor, Const}
//import java.util.concurrent.TimeUnit.{MILLISECONDS ⇒ MS}
//import scala.reflect.runtime.universe.TypeTag
//
//  /** Operations that work only on event streams */
//  implicit class EFOps[R,A](val s: EF[R,A]) extends AnyVal {
//
//    /** Map and filter an event stream in one run */
//    def collect[B](f: A ⇒ Option[B]): EF[R,B] =
//      sync2(s, never)(Change collectI f)(Change collectN f)
//
//    /** Counts the number of events this event stream fired and
//      * stores the results in a signal
//      */
//    def count: SF[R,Int] = scanMap { _ ⇒ 1 }
//
//    /** Filters an event stream according to the given predicate */
//    def filter(p: A ⇒ Boolean): EF[R,A] =
//      collect[A] { a ⇒ p(a) option a }
//
//    /** Converts this event stream to a signal with initial value
//      * `ini`
//      */
//    def hold[B>:A](ini: B): SF[R,B] = scan[B](ini)((next,_) ⇒ next)
//
//    /** Returns an event stream that fires whenever one of the input streams
//      * fire.
//      *
//      * If both event streams fire at the same time, the event of the
//      * second (right) stream is ignored
//      */
//    def merge(that: EF[R,A]): EF[R,A] =
//      sync2(s, that)(Change.mergeI)(Change.mergeN)
//
//    /** Accumulates events fired by this event stream in a signal
//      *
//      * If the original event stream fires its first event 'a' at time
//      * 'T0', the resulting signal's initial value will be
//      * 'next(a, ini)', otherwise it will be just 'ini'
//      *
//      * This is the most fundamental function for accumulating the
//      * events of an event stream in a signal. All other functions like
//      * 'hold', 'scanMap', and so on can be derrived from this one
//      *
//      * @param ini  the initial value of the resulting signal
//      * @param next combines the fired event with the
//      *             value accumulated so far
//      */
//    def scan[B](ini: ⇒ B)(next: (A,B) ⇒ B): SF[R,B] = 
//      sync2(s, never)(Change.scanI(ini)(next))(Change.scanN(ini)(next))
//
//    /** Accumulates events by first transferring them to a value
//      * with a Monoid instance
//      */
//    def scanMap[B:Monoid](f: A ⇒ B): SF[R,B] = scan(∅[B])((a,b) ⇒ b ⊹ f(a))
//
//    /** Accumulates events in a container */
//    def scanPlus[F[_]](implicit P: ApplicativePlus[F]): SF[R,F[A]] = 
//      scanMap(_.η[F])(P.monoid)
//
//    /** Accumulates the results of a stateful calculation */
//    def scanSt[S,B](ini: S)(implicit w: A <:< State[S,B]): EF[R,(S,B)] =
//      scan[Event[(S,B)]](Never)((s: A, e: Event[(S,B)]) ⇒ 
//        e map { s run _._1 } orElse Once(s run ini))
//
//    /** Accumulates the results of a stateful calculation in a signal
//      * starting at value `ini`.
//      *
//      * Note that the stateful calculation is performed purely for its
//      * effects on the state and the result is discarded.
//      */
//    def scanStHold[S,B](ini: S)(implicit w: A <:< State[S,B]): SF[R,S] =
//      scanStS[S,B](ini) hold ini
//
//    /** Accumulates the results of a stateful calculation 
//      * keeping the state and discarding the result
//      */
//    def scanStS[S,B](ini: S)(implicit w: A <:< State[S,B]): EF[R,S] =
//      scanSt[S,B](ini) mapE { _._1 }
//
//    /** Accumulates the results of a stateful calculation 
//      * discarding the new state
//      */
//    def scanStV[S,B](ini: S)(implicit w: A <:< State[S,B]): EF[R,B] =
//      scanSt[S,B](ini) mapE { _._2 }
//
//    /** Accumulates events using a Monoid */
//    def sum(implicit M: Monoid[A]): SF[R,A] = scanMap(identity)
//  }
//}
//
//trait SFInstances extends EFInstances {
//  //Type class implementations
//
//  implicit val SFArrow: Arrow[SF] = new Arrow[SF] {
//    def id[A]: SF[A,A] = SF { ra ⇒ _ ⇒ IO(ra) }
//
//    def arr[A,B](f: A ⇒ B): SF[A,B] = SF { ra ⇒ r ⇒ 
//      RS.sync2(ra, Const(()))(Change mapI f)(Change mapN f) }
//
//    def compose[A,B,C](f: SF[B,C], g: SF[A,B]) = f compose g
//
//    def first[A,B,C](f: SF[A,B]): SF[(A,C),(B,C)] = {
//      val sfAC = id[(A,C)]
//      val sfACB = sfAC map { _._1 } andThen f
//      val sfACC = sfAC map { _._2 }
//
//      (sfACB <*> sfACC){ Tuple2.apply }
//    }
//  }
//
//  implicit def SFApplicative[R]: Applicative[({type λ[α]=SF[R,α]})#λ] =
//    new Applicative[({type λ[α]=SF[R,α]})#λ] {
//      def point[A](a: ⇒ A) = SF const a
//      def ap[A,B](a: ⇒ SF[R,A])(f: ⇒ SF[R,A ⇒ B]) = a ap f
//    }
//}
//
//trait SFFunctions {
//  lazy val ms = 1000L
//
//  lazy val s = ms * 1000L
//
//  lazy val min = s * 60L
//
//  lazy val h = min * 60L
//
//  /** The time signal
//    *
//    * In every isolated reactive system there is only one such signal
//    */
//  def time: SIn[Time] = SF { _ ⇒ _.timeSignal }
//
//  def seconds: SIn[Int] = time.events filter { _ % s == 0L } count
//
//  def minutes: SIn[Int] = time.events filter { _ % min == 0L } count
//
//  def hours: SIn[Int] = time.events filter { _ % h == 0L } count
//
//  /** Creates an input Signal from an external data source
//    *
//    * See the [[dire.DataSource]] type class for more details
//    */
//  def src[S,V](s: S)(implicit Src: DataSource[S,V]): SIn[V] =
//    SF(_ ⇒ _.source(Src ini s)(Src cb s))
//
//  /** Creates an input Signal from an external data source
//    *
//    * Unlike src, the resulting signal function is cached using `s`
//    * as a key.
//    */
//  def cachedSrc[S,V:TypeTag](s: S)(implicit Src: DataSource[S,V]): SIn[V] =
//    cached(src[S,V](s), s)
//
//  /** Creates a data sink, that consumes data but never fires
//    * an event.
//    */
//  def sink[S,A](s: S)(implicit D: DataSink[S,A]): EF[A,Nothing] = 
//    id[A] toSink s andThen never
//
//  /** Asynchronuously loops back the output of the given
//    * signal function to its input
//    */
//  def loop[A](sf: SF[A,A])(ini: ⇒ A): SIn[A] = 
//    SF { _ ⇒ r ⇒ r.loop(ini)(sf) }
//
//  /** Sometimes, part of a reactive graph appears in several
//    * places in the description of the reactive graph.
//    *
//    * Caching such a sub-graph using the given `tag` guarantees,
//    * that the result of the given signal function with the
//    * same type of input and output parameters and the same `tag`
//    * is only created once when setting up the reactive graph.
//    * Additional calls to the resulting signal-functions `run`
//    * method will result in the cached `RawSignal` being returned.
//    */
//  def cached[A:TypeTag,B:TypeTag](sf: SF[A,B], tag: Any): SF[A,B] =
//    SF(ra ⇒ _.cached[A,B](sf run ra, tag))
//
//  /** A constant signal that never changes */
//  def const[A,B](b: ⇒ B): SF[A,B] = SF(_ ⇒ _ ⇒ RS const b)
//
//  def sf[A,B,S](s: S)(implicit Si: DataSink[S,A],
//                               So: DataSource[S,B]): SF[A,B] =
//    id[A] toSink s andThen src(s)
//
//  def sfCached[A:TypeTag,B:TypeTag,S]
//    (s: S)(implicit Si: DataSink[S,A], So: DataSource[S,B]): SF[A,B] =
//    cached(sf(s), s)
//
//  /** Sets up a reactive network and runs it until the given
//    * abort condition is fullfilled.
//    *
//    * @param in  The signal function that describes the reactive
//    *            network
//    *
//    * @param step  time resolution in microseconds. If the given
//    *              signal function depends on the 'Time' signal
//    *              this number denotes how frequently 'Time' is
//    *              being updated. The default value is '1000L'
//    *              (one millisecond)
//    *
//    * @param proc Number of processors (threads) available to the
//    *             actors running in the background. The default
//    *             value is the total number of processors the
//    *             application is running on
//    *
//    * @param stop This function should return true when a certain
//    *             abbort condition is reached. In that case, the
//    *             reactive framework will cease to run and release
//    *             all its resources. Note that the framework will
//    *             stop immediately AFTER 'stop' has returned true
//    *             but that the event that lead to abortion
//    *             is still processed and passed to all
//    *             registered data sinks.
//    */
//  def run[A](in: SIn[A],
//             proc: Int = processors,
//             step: Time = 1000L)
//            (stop: A ⇒ Boolean): IO[Unit] = {
//
//      lazy val ex = java.util.concurrent.Executors.newFixedThreadPool(proc)
//      lazy val s = scalaz.concurrent.Strategy.Executor(ex)
//      var doKill = false
//      val cdl = new java.util.concurrent.CountDownLatch(1)
//
//      for {
//        r ← IO(new Reactor(step, () ⇒ doKill, cdl, s))
//        _ ← in.syncTo { stop(_) ? IO{doKill = true} | IO.ioUnit }
//              .run(Const(()))
//              .apply(r)
//        _ ← r.start
//        _ ← IO { cdl.await()
//                 //ex.awaitTermination(1L, MS)
//                 ex.shutdown() }
//      } yield ()
//    }
//}

// vim: set ts=2 sw=2 et:
