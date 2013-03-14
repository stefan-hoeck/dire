package dire.model

import dire.{Time, Event}
import scalaz._, Scalaz._

sealed trait Signal[+A] {
  def es: Nel[Event[A]]
  def events = es.list

  /** Value of this signal at time t
    *
    * Fires an exception if time t is smaller that the time of
    * the first event in this Signal. In a controlled reactive
    * framework, such a thin should never happen. Time must be
    * strictly increasing, and a Signal must start with an
    * event that happened at the time when the Signal was
    * created.
    *
    * This invariant will have to be considered when designing
    * an effectful Monad used to initialize a reactive Graph
    */
  def valueAt(t: Time): A = (events find { _.at <= t }).get.v

  /** Actual value of this signal */
  def now: A = es.head.v

  def map[B](f: A ⇒ B): Signal[B] = Var(es map { _ map f })

  /** Applies functions in a signal to the events of this
    * signal
    */
  def ap[B](f: Signal[A ⇒ B]): Signal[B] =
    Event.combine(events, f.events)((a,f) ⇒ f(a)) match {
      case (h :: t) ⇒ Var(nel(h, t))
      case _        ⇒ sys.error("What!?")
    }
}

/** A changing signal */
case class Var[+A](es: Nel[Event[A]]) extends Signal[A]

/** A constant signal */
case class Val[+A](a: Event[A]) extends Signal[A] {
  def es = nel(a, Nil)
}

object Signal extends SignalInstances

trait SignalInstances {
  implicit def SignalEqual[A:Equal]: Equal[Signal[A]] =
    Equal.equalBy(_.events)

  implicit def SignalApplicative: Applicative[Signal] =
    new Applicative[Signal] {
      def ap[A,B](a: ⇒ Signal[A])(f: ⇒ Signal[A ⇒ B]) = a ap f
      def point[A](a: ⇒ A) = Val(Event(0L, a))
    }
}

// vim: set ts=2 sw=2 et:
