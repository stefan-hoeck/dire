package dire

//import dire.control.{RawSignal, Reactor}
//import Change.{InitialS, NextS}
//import scalaz._, Scalaz._, effect.IO
//
//final case class Signal[+A](run: Reactor ⇒ IO[RawSignal[A]]) {
//  import Signal.{sync2, never}
//
//  /** Applies a time-changing function to the signals values */
//  def ap[B](f: Signal[A ⇒ B]): Signal[B] = (f <*> this)(_ apply _)
//
//  /** Creates an event stream that fires whenever this signal
//    * changes.
//    *
//    * No event is fired when the signal is initialized.
//    */
//  def changes[B>:A](implicit E: Equal[B]): Events[B] =
//    sync2(this, never)(Change.changesI[B])(Change.changesN[B])
//   
//  /** Functor map */
//  def map[B](f: A ⇒ B): Signal[B] =
//    sync2(this, never)(Change mapI f)(Change mapN f)
//
//  /** Performs the given side-effect whenever this signal changes.
//    *
//    * The side effect is also performed with the signals initial value
//    */
//  def to(out: Out[A]): Signal[A] =
//    Signal { run(_) >>= { rs ⇒ rs onChange out as rs } }
//
//  /** Combines two signals via a pure function */
//  def <*>[B,C](that: Signal[B])(f: (A,B) ⇒ C): Signal[C] =
//    sync2(this, that)(Change applyI f)(Change applyN f)
//
//  /** Alias for 'to' */
//  def -->(out: Out[A]): Signal[A] = to(out)
//}
//
//object Signal extends SignalFunctions with SignalInstances {
//
//  implicit class EventsOps[A](val s: Events[A]) extends AnyVal {
//    /** Map and filter an event stream in one run */
//    def collect[B](f: A ⇒ Option[B]): Events[B] =
//      sync2(s, never)(Change collectI f)(Change collectN f)
//
//    /** Performs the given side-effect whenever this event stream
//      * fires an event */
//    def eventTo(out: Out[A]): Events[A] =
//      s to { _ fold (out, IO.ioUnit) }
//
//    /** Filters an event stream according to the given predicate */
//    def filter(p: A ⇒ Boolean): Events[A] =
//      collect[A] { a ⇒ p(a) option a }
//
//    /** Returns an event stream that fires whenever one of the input streams
//      * fire.
//      *
//      * If both event streams fire at the same time, the event of the
//      * second (right) stream is ignored
//      */
//    def merge[B>:A](that: Events[B]): Events[B] =
//      sync2(s, that)(Change.mergeI)(Change.mergeN)
//
//    /** Alias for 'eventTo' */
//    def --?>(out: Out[A]): Events[A] = eventTo(out)
//  }
//
//}
//
//trait SignalInstances {
//  implicit val SignalApplicative: Applicative[Signal] =
//    new Applicative[Signal] {
//      def point[A](a: ⇒ A) = Signal const a
//      def ap[A,B](a: ⇒ Signal[A])(f: ⇒ Signal[A ⇒ B]) = a ap f
//    }
//
//  implicit val EventsPlus: PlusEmpty[Events] = new PlusEmpty[Events] {
//    def empty[A] = Signal.never
//    def plus[A](a: Events[A], b: ⇒ Events[A]) = a merge b
//  }
//
//  implicit def EventsMonoid[A]: Monoid[Events[A]] = EventsPlus.monoid
//}
//
//trait SignalFunctions {
//  /** The event streams that never fires */
//  val never: Events[Nothing] = const(Never)
//
//  /** A constant signal that never changes */
//  def const[A](a: ⇒ A): Signal[A] = Signal(_ ⇒ RawSignal const a)
//
//  /** Creates a derrived signal depending on two input signals
//    * that is synchronously updated whenever one of the two
//    * input signals changes
//    */
//  private[dire] def sync2[A,B,C](sa: Signal[A], sb: Signal[B])
//                                (ini: InitialS[A,B,C])
//                                (next: NextS[A,B,C]): Signal[C] =
//    Signal[C] { r ⇒ 
//      for {
//        ra ← sa run r
//        rb ← sb run r
//        rc ← RawSignal.sync2(ra, rb)(ini)(next)
//      } yield rc
//    }
//}

// vim: set ts=2 sw=2 et:
