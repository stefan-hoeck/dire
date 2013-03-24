package dire

import Change.{InitialS, NextS}
import dire.control.{RawSignal ⇒ RS, Reactor}
import java.util.concurrent.TimeUnit.{MILLISECONDS ⇒ MS}
import scala.reflect.runtime.universe.TypeTag
import scalaz._, Scalaz._, effect.IO

/** Represents a signal transformation.
  *
  * This class offers a rich set of combinators needed to describe
  * a reactive network.
  */
case class SF[-A,+B](run: RS[A] ⇒ Signal[B]) {
  import SF.{sync2, never}

  /** Sequentially combines two signal functions */
  def andThen[C](that: SF[B,C]): SF[A,C] = that compose this

  /** Applies a time-changing function to the signals values */
  def ap[C,D<:A](f: SF[D,B ⇒ C]): SF[D,C] = (f <*> this)(_ apply _)

  /** Connect a reactive branch to this signal function but
    * return to the original branch afterwards.
    */
  def branch[C](that: SF[B,C]): SF[A,B] = 
    SF { ra ⇒ r ⇒ run(ra)(r) >>= { rb ⇒ that.run(rb)(r) as rb } }

  /** Creates an event stream that fires whenever this signal
    * changes.
    *
    * No event is fired when the signal is initialized.
    */
  def changes[C>:B](implicit E: Equal[C]): SF[A,Event[C]] =
    sync2(this, never)(Change.changesI[C])(Change.changesN[C])

  private[dire] def changeTo(out: Out[Change[B]]): SF[A,B] =
    SF { ra ⇒ r ⇒ run(ra)(r) >>= { rb ⇒ rb onChange out as rb } }

  /** Sequentially combines two signal functions */
  def compose[C](that: SF[C,A]): SF[C,B] =
    SF { rc ⇒ r ⇒ that.run(rc)(r) >>= { run(_)(r) } }
   
  /** Functor map */
  def map[C](f: B ⇒ C): SF[A,C] =
    sync2(this, never)(Change mapI f)(Change mapN f)

  /** Performs the given side-effect whenever this signal changes.
    *
    * The side effect is also performed with the signals initial value
    */
  def to(out: Out[B]): SF[A,B] = changeTo(c ⇒ out(c.v))

  /** Combines two signals via a pure function */
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
      * stores the numbers in a signal
      */
    def count: SF[R,Int] = scanMap { _ ⇒ 1 }

    /** Performs the given side-effect whenever this event stream
      * fires an event */
    def eventTo(out: Out[A]): EF[R,A] =
      s to { _ fold (out, IO.ioUnit) }

    /** Filters an event stream according to the given predicate */
    def filter(p: A ⇒ Boolean): EF[R,A] =
      collect[A] { a ⇒ p(a) option a }

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

    /** Accumulates events using a Monoid */
    def sum(implicit M: Monoid[A]): SF[R,A] = scanMap(identity)

    /** Alias for 'eventTo' */
    def --?>(out: Out[A]): EF[R,A] = eventTo(out)
  }
}

trait SFInstances {
  //Type class implementations

  implicit val SFArrow: Arrow[SF] = new Arrow[SF] {
    def id[A]: SF[A,A] = SF { ra ⇒ _ ⇒ IO(ra) }

    def arr[A,B](f: A ⇒ B): SF[A,B] = SF { ra ⇒ r ⇒ 
      RS.sync2(ra, RS.Const(()))(Change mapI f)(Change mapN f) }

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
  import SourceSignal._

  private lazy val processors =
    Runtime.getRuntime.availableProcessors

  /** The time signal
    *
    * In every isolated reactive system there is only one such signal
    */
  def time(step: Time): SIn[Time] = 
    cached(src[Time,Time](step), "DireCoreTime")

  def ticks(step: Time): EIn[Unit] = src[Time,Event[Unit]](step)

  def src[S,V](s: S)(implicit Src: SourceSignal[S,V]): SIn[V] =
    SF(_ ⇒ _.sourceSignal(s))

  /** The event stream that never fires */
  def never[A]: EF[A,Nothing] = const(Never)

  def cached[A:TypeTag,B:TypeTag](sf: SF[A,B], tag: Any): SF[A,B] =
    SF(ra ⇒ _.cached[A,B](sf run ra, tag))

  /** A constant signal that never changes */
  def const[A,B](b: ⇒ B): SF[A,B] = SF(_ ⇒ _ ⇒ RS const b)

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
    * @param next  calculates the new value of the derrived
    *              signal from the two actual values of the
    *              input signals and the derrived signal's
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
    *             but that this final event that lead to abortion
    *             is still copletely processed
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
        _ ← in.to { stop(_) ? IO(doKill = true) | IO.ioUnit }
              .run(RS Const ())
              .apply(r)
        _ ← r.start
        _ ← IO { cdl.await()
                 ex.awaitTermination(10L, MS)
                 ex.shutdown() }
      } yield ()
    }

  def runE[A](in: EIn[A],
              proc: Int = processors)
             (stop: A ⇒ Boolean): IO[Unit] =
    runS[Event[A]](in, proc)(_ fold (stop, false))
}

// vim: set ts=2 sw=2 et:
