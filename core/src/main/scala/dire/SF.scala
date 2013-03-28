package dire

import Change.{InitialS, NextS}
import dire.control.{RawSignal ⇒ RS, Reactor, Const}
import java.util.concurrent.TimeUnit.{MILLISECONDS ⇒ MS}
import scala.reflect.runtime.universe.TypeTag
import scalaz._, Scalaz._, effect.IO

/** Represents a signal transformation.
  *
  * This class offers a rich set of combinators needed to describe
  * reactive networks.
  */
case class SF[-A,+B](run: RS[A] ⇒ Signal[B]) {
  import SF.{sync2, never}

  /** Sequentially combines two signal functions */
  def andThen[C](that: SF[B,C]): SF[A,C] = that compose this

  /** Applies a time-changing function to the signals values */
  def ap[C,D<:A](f: SF[D,B ⇒ C]): SF[D,C] = (f <*> this)(_ apply _)

  /** Like `to` but performs side effects asynchronuously,
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
  def asyncTo(out: Out[B]): SF[A,B] =
    toSink(())(DataSink.create[Unit,B](_ ⇒  out, _ ⇒ IO.ioUnit))

  /** Connect a reactive branch to this signal function but
    * return to the original branch afterwards.
    */
  def branch[C](that: SF[B,C]): SF[A,B] = 
    SF { ra ⇒ r ⇒ run(ra)(r) >>= { rb ⇒ that.run(rb)(r) as rb } }

  /** Returns a signal that only fires an event if its new
    * value is different from its old one
    */
  def distinct[C>:B:Equal]: SF[A,C] =
    sync2(this, never)(Change.distinctI[C])(Change.distinctN[C])

  /** Creates an event stream that fires whenever this signal's
    * value changes to a new one that is distinct from the old
    * value.
    */
  def changes[C>:B:Equal]: SF[A,Event[C]] = distinct[C].events

  private[dire] def changeTo(out: Out[Change[B]]): SF[A,B] =
    toSink(())(DataSink synchC out)

  /** Creates an event stream that fires whenever this signal
    * fires an event.
    *
    * This is similar to `changes` but fires even if the signal's
    * new value is not distinct from its old value
    */
  def events: SF[A,Event[B]] =
    sync2(this, never)(Change.eventsI[B])(Change.eventsN[B])

  /** Sequentially combines two signal functions */
  def compose[C](that: SF[C,A]): SF[C,B] =
    SF { rc ⇒ r ⇒ that.run(rc)(r) >>= { run(_)(r) } }
   
  /** Functor map */
  def map[C](f: B ⇒ C): SF[A,C] =
    sync2(this, never)(Change mapI f)(Change mapN f)

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
  def to(out: Out[B]): SF[A,B] = changeTo(c ⇒ out(c.v))

  /** Asynchronuously output the values of this Signal to a data sink
    *
    * How the data sink operates and what concurrency strategy is
    * applied is defined in the [[dire.DataSink]] type class.
    */
  def toSink[S](s: S)(implicit D: DataSink[S,B]): SF[A,B] = 
    SF { ra ⇒ r ⇒ run(ra)(r) >>= { r.sink(s, _) } }

  /** Combines two signals with a pure function */
  def <*>[C,D,E<:A](that: SF[E,C])(f: (B,C) ⇒ D): SF[E,D] =
    sync2(this, that)(Change applyI f)(Change applyN f)

  /** Alias for 'to' */
  def -->(out: Out[B]): SF[A,B] = to(out)

}

object SF extends SFInstances with SFFunctions {

  /** Operations that work only on event streams */
  implicit class EventsOps[R,A](val s: EF[R,A]) extends AnyVal {

    /** Map and filter an event stream in one run */
    def collect[B](f: A ⇒ Option[B]): EF[R,B] =
      sync2(s, never)(Change collectI f)(Change collectN f)

    /** Counts the number of events this event stream fired and
      * stores the results in a signal
      */
    def count: SF[R,Int] = scanMap { _ ⇒ 1 }

    /** Performs the given side-effect whenever this event stream
      * fires an event 
      *
      * This is similar to `SF.to`, i.e. side effects are performed
      * in the `Reactor`'s main worker thread, thus blocking the
      * reactive system.
      */
    def eventTo(out: Out[A]): EF[R,A] =
      s to { _ fold (out, IO.ioUnit) }

    /** Filters an event stream according to the given predicate */
    def filter(p: A ⇒ Boolean): EF[R,A] =
      collect[A] { a ⇒ p(a) option a }

    /** Converts this event stream to a signal with initial value
      * `ini`
      */
    def hold[B>:A](ini: B): SF[R,B] = scan[B](ini)((next,_) ⇒ next)

    /** Functor map for event streams */
    def mapE[B](f: A ⇒ B): EF[R,B] = collect(f(_).some)

    /** Returns an event stream that fires whenever one of the input streams
      * fire.
      *
      * If both event streams fire at the same time, the event of the
      * second (right) stream is ignored
      */
    def merge(that: EF[R,A]): EF[R,A] =
      sync2(s, that)(Change.mergeI)(Change.mergeN)

    /** Accumulates events fired by this event stream in a signal
      *
      * If the original event stream fires its first event 'a' at time
      * 'T0', the resulting signal's initial value will be
      * 'next(a, ini)', otherwise it will be just 'ini'
      *
      * This is the most fundamental function for accumulating the
      * events of an event stream in a signal. All other functions like
      * 'hold', 'scanMap', and so on can be derrived from this one
      *
      * @param ini  the initial value of the resulting signal
      * @param next combines the fired event with the
      *             value accumulated so far
      */
    def scan[B](ini: ⇒ B)(next: (A,B) ⇒ B): SF[R,B] = 
      sync2(s, never)(Change.scanI(ini)(next))(Change.scanN(ini)(next))

    /** Accumulates events by first transferring them to a value
      * with a Monoid instance
      */
    def scanMap[B:Monoid](f: A ⇒ B): SF[R,B] = scan(∅[B])((a,b) ⇒ b ⊹ f(a))

    /** Accumulates events in a container */
    def scanPlus[F[_]](implicit P: ApplicativePlus[F]): SF[R,F[A]] = 
      scanMap(_.η[F])(P.monoid)

    /** Accumulates the results of a stateful calculation */
    def scanSt[S,B](ini: S)(implicit w: A <:< State[S,B]): EF[R,(S,B)] =
      scan[Event[(S,B)]](Never)((s: A, e: Event[(S,B)]) ⇒ 
        e map { s run _._1 } orElse Once(s run ini))

    /** Accumulates the results of a stateful calculation in a signal
      * starting at value `ini`.
      *
      * Note that the stateful calculation is performed purely for its
      * effects on the state and the result is discarded.
      */
    def scanStHold[S,B](ini: S)(implicit w: A <:< State[S,B]): SF[R,S] =
      scanStS[S,B](ini) hold ini

    /** Accumulates the results of a stateful calculation 
      * keeping the state and discarding the result
      */
    def scanStS[S,B](ini: S)(implicit w: A <:< State[S,B]): EF[R,S] =
      scanSt[S,B](ini) mapE { _._1 }

    /** Accumulates the results of a stateful calculation 
      * discarding the new state
      */
    def scanStV[S,B](ini: S)(implicit w: A <:< State[S,B]): EF[R,B] =
      scanSt[S,B](ini) mapE { _._2 }

    /** Accumulates events using a Monoid */
    def sum(implicit M: Monoid[A]): SF[R,A] = scanMap(identity)

    /** Alias for 'eventTo' */
    def --?>(out: Out[A]): EF[R,A] = eventTo(out)
  }
}

trait SFInstances extends SFInstances0 {

  implicit def EFFunctor[R]: Functor[({type λ[α]=EF[R,α]})#λ] =
    new Functor[({type λ[α]=EF[R,α]})#λ] {
      def map[A,B](a: EF[R,A])(f: A ⇒ B) = a mapE f
    }

}

trait SFInstances0 {
  //Type class implementations

  implicit val SFArrow: Arrow[SF] = new Arrow[SF] {
    def id[A]: SF[A,A] = SF { ra ⇒ _ ⇒ IO(ra) }

    def arr[A,B](f: A ⇒ B): SF[A,B] = SF { ra ⇒ r ⇒ 
      RS.sync2(ra, Const(()))(Change mapI f)(Change mapN f) }

    def compose[A,B,C](f: SF[B,C], g: SF[A,B]) = f compose g

    def first[A,B,C](f: SF[A,B]): SF[(A,C),(B,C)] = {
      val sfAC = id[(A,C)]
      val sfACB = sfAC map { _._1 } andThen f
      val sfACC = sfAC map { _._2 }

      (sfACB <*> sfACC){ Tuple2.apply }
    }
  }

  implicit def SFApplicative[R]: Applicative[({type λ[α]=SF[R,α]})#λ] =
    new Applicative[({type λ[α]=SF[R,α]})#λ] {
      def point[A](a: ⇒ A) = SF const a
      def ap[A,B](a: ⇒ SF[R,A])(f: ⇒ SF[R,A ⇒ B]) = a ap f
    }

  implicit def EFPlus[R]: PlusEmpty[({type λ[α]=EF[R,α]})#λ] =
    new PlusEmpty[({type λ[α]=EF[R,α]})#λ] {
      def empty[A] = SF.never
      def plus[A](a: EF[R,A], b: ⇒ EF[R,A]) = a merge b
    }

  implicit def EFMonoid[A,B]: Monoid[EF[A,B]] = EFPlus[A].monoid
}

trait SFFunctions {
  import DataSource._

  private lazy val processors =
    Runtime.getRuntime.availableProcessors

  /** The time signal
    *
    * In every isolated reactive system there is only one such signal
    */
  def time(step: Time): SIn[Time] = 
    cached(src[Time,Time](step), "DireCoreTime")

  /** An asynchronous event source that fires at regular
    * intervals.
    *
    * This is very useful as a basic source of events to simulate
    * all kinds of real time applications.
    * Note that an arbitrary number of completely independant
    * event streams can thus be created. 
    */
  def ticks(step: Time): EIn[Unit] = src[Time,Event[Unit]](step)

  /** Creates an input Signal from an external data source
    *
    * See the [[dire.DataSource]] type class for more details
    */
  def src[S,V](s: S)(implicit Src: DataSource[S,V]): SIn[V] =
    SF(_ ⇒ _.source(s))

  /** The event stream that never fires */
  def never[A]: EF[A,Nothing] = const(Never)

  /** Asynchronuously fires the given event once */
  def once[A](a: ⇒ A): EIn[A] = src(())(DataSource once a)

  /** Asynchronuously loops back the output of the given
    * signal function to its input
    */
  def loop[A](sf: SF[A,A])(ini: ⇒ A): SIn[A] = 
    SF { _ ⇒ r ⇒ r.loop(ini)(sf) }

  /** Asynchronuously loops back the output of the given
    * event stream to its input
    */
  def loopE[A](ef: EF[Event[A],A]): EIn[A] = loop(ef)(Never)

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
    SF(ra ⇒ _.cached[A,B](sf run ra, tag))

  /** A constant signal that never changes */
  def const[A,B](b: ⇒ B): SF[A,B] = SF(_ ⇒ _ ⇒ RS const b)

  def id[A]: SF[A,A] = Arrow[SF].id[A]

  def sf[A,B,S](s: S)(implicit Si: DataSink[S,A],
                               So: DataSource[S,B]): SF[A,B] =
    id[A] toSink s andThen src(s)

  def sfCached[A:TypeTag,B:TypeTag,S]
    (s: S)(implicit Si: DataSink[S,A], So: DataSource[S,B]): SF[A,B] =
    cached(sf(s), s)

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
  private[dire] def sync2[R,A,B,C](sa: SF[R,A], sb: SF[R,B])
                                  (ini: InitialS[A,B,C])
                                  (next: NextS[A,B,C]): SF[R,C] =
    SF[R,C] { rr ⇒ r ⇒ 
      for {
        ra ← sa.run(rr)(r)
        rb ← sb.run(rr)(r)
        rc ← RS.sync2(ra, rb)(ini)(next)
      } yield rc
    }

  /** Sets up a reactive network and runs it until the given
    * abort condition is fullfilled.
    *
    * @param in  The signal function that describes the reactive
    *            network
    *
    * @param step  time resolution in microseconds. If the given
    *              signal function depends on the 'Time' signal
    *              this number denotes how frequently 'Time' is
    *              being updated. The default value is '1000L'
    *              (one millisecond)
    *
    * @param proc Number of processors (threads) available to the
    *             actors running in the background. The default
    *             value is the total number of processors the
    *             application is running on
    *
    * @param stop This function should return true when a certain
    *             abbort condition is reached. In that case, the
    *             reactive framework will cease to run and release
    *             all its resources. Note that the framework will
    *             stop immediately AFTER 'stop' has returned true
    *             but that the event that lead to abortion
    *             is still processed and passed to all
    *             registered data sinks.
    */
  def runS[A](in: SIn[A],
              proc: Int = processors)
              (stop: A ⇒ Boolean): IO[Unit] = {

      lazy val ex = java.util.concurrent.Executors.newFixedThreadPool(proc)
      lazy val s = scalaz.concurrent.Strategy.Executor(ex)
      var doKill = false
      val cdl = new java.util.concurrent.CountDownLatch(1)

      for {
        r ← IO(new Reactor(() ⇒ doKill, cdl, s))
        _ ← in.to { stop(_) ? IO{doKill = true} | IO.ioUnit }
              .run(Const(()))
              .apply(r)
        _ ← r.start
        _ ← IO { cdl.await()
                 //ex.awaitTermination(1L, MS)
                 ex.shutdown() }
      } yield ()
    }

  def runE[A](in: EIn[A],
              proc: Int = processors)
             (stop: A ⇒ Boolean): IO[Unit] =
    runS[Event[A]](in, proc)(_ fold (stop, false))
}

// vim: set ts=2 sw=2 et:
