package dire.model

import dire.{Time, Event, T0}
import scalaz._, Scalaz._

sealed trait Signal[+A] extends Function1[Time,A]{
  import Signal.Impl

  def init: Event[A]

  def changes: List[Event[A]]

  final def events: List[Event[A]] = init :: changes

  /** Value of this signal at time t
    *
    * Since Signals represent total functions, the initial event always
    * happened at `Time T0`. If a `Signal` should represent
    * a function whose value is undefined at `T0` or other times,
    * it should hold values of type `Option[A]`.
    */
  def apply(t: Time): A = changes.find { _.at >= t } cata (_.v, now)

  /** Actual value of this signal */
  lazy val now: A = events.last.v

  def map[B](f: A ⇒ B): Signal[B] =
    Impl(init map f, changes map { _ map f})

  /** Applies functions in a signal to the events of this
    * signal
    */
  def ap[B](f: Signal[A ⇒ B]): Signal[B] = {
    def ts[X](s: Signal[X]) = s.changes map { _.at }
    def atT(t: Time) = f(t) apply this(t)

    Signal(atT(T0), (ts(this) ::: ts(f)) map { t ⇒ t → atT(t) } toMap)
  }
}

object Signal extends SignalInstances {
  def apply[A](ini: A, events: Map[Time,A]): Signal[A] =
    Impl(Event(T0, ini),
         events.toList map { case (t,a) ⇒ Event(t,a) } sortBy { _.at })

  private case class Impl[+A](init: Event[A], changes: List[Event[A]])
    extends Signal[A]
}

trait SignalInstances {

  implicit def SignalApplicative: Applicative[Signal] =
    new Applicative[Signal] {
      def ap[A,B](a: ⇒ Signal[A])(f: ⇒ Signal[A ⇒ B]) = a ap f
      def point[A](a: ⇒ A) = Signal(a, Map.empty)
    }
}

// vim: set ts=2 sw=2 et:
