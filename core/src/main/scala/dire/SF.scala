package dire

import Change.{InitialS, NextS}
import dire.control.{RawSignal ⇒ RS, Reactor}
import java.util.concurrent.TimeUnit.{MILLISECONDS ⇒ MS}
import scalaz._, Scalaz._, effect.IO

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

  implicit class EventsOps[R,A](val s: EF[R,A]) extends AnyVal {

    /** Map and filter an event stream in one run */
    def collect[B](f: A ⇒ Option[B]): EF[R,B] =
      sync2(s, never)(Change collectI f)(Change collectN f)

    /** Performs the given side-effect whenever this event stream
      * fires an event */
    def eventTo(out: Out[A]): EF[R,A] =
      s to { _ fold (out, IO.ioUnit) }

    /** Filters an event stream according to the given predicate */
    def filter(p: A ⇒ Boolean): EF[R,A] =
      collect[A] { a ⇒ p(a) option a }

    /** Returns an event stream that fires whenever one of the input streams
      * fire.
      *
      * If both event streams fire at the same time, the event of the
      * second (right) stream is ignored
      */
    def merge(that: EF[R,A]): EF[R,A] =
      sync2(s, that)(Change.mergeI)(Change.mergeN)

    /** Alias for 'eventTo' */
    def --?>(out: Out[A]): EF[R,A] = eventTo(out)
  }
}

trait SFInstances {
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
  private lazy val processors =
    Runtime.getRuntime.availableProcessors

  /** The time signal
    *
    * In every isolated reactive system there is only one such signal
    */
  val time: SIn[Time] = SF(_ ⇒ _.getTimer)

  /** The event streams that never fires */
  def never[A]: EF[A,Nothing] = const(Never)

  /** A constant signal that never changes */
  def const[A,B](b: ⇒ B): SF[A,B] = SF(_ ⇒ _ ⇒ RS const b)

  /** Creates a derrived signal depending on two input signals
    * that is synchronously updated whenever one of the two
    * input signals changes
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

  def runReactive[A](in: SIn[A],
                     step: Time = 1000L,
                     proc: Int = processors)
                    (stop: A ⇒ Boolean): IO[Unit] = {
      lazy val ex = java.util.concurrent.Executors.newFixedThreadPool(proc)
      lazy val s = scalaz.concurrent.Strategy.Executor(ex)
      //lazy val s = scalaz.concurrent.Strategy.Sequential

      var doKill = false
      val cdl = new java.util.concurrent.CountDownLatch(1)

      for {
        r ← IO(new Reactor(step, () ⇒ doKill, cdl, s))
        _ ← in.to { stop(_) ? IO(doKill = true) | IO.ioUnit }
              .run(RS Const () )
              .apply(r)
        _ ← r.start
        _ ← IO { cdl.await()
                 ex.awaitTermination(10L, MS)
                 ex.shutdown()
               }
      } yield ()
    }
}

// vim: set ts=2 sw=2 et:
