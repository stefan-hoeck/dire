package dire.model

import dire.{Time, Event}
import scalaz._, Scalaz._

sealed trait Events[+A] {
  import Events.Impl

  /** A list of events in increasing time order */
  def events: List[Event[A]]

  /** Returns all events of this event stream that happened
    * at time t
    *
    * An event stream might hold several events that happened
    * at the same time. We must make sure that there is a notion
    * of ordering among such events, and that this ordering is
    * preserved when event streams are merged. The moste simple idea
    * is, that events of the left (or the right) stream take
    * precedence when merging.
    *
    * Note: This will actually be easier to implement when we only
    * keep track of the latest events. No sorting will be necessary
    * then
    */ 
  def at(t: Time): List[A] = events collect { case Event(`t`,v) ⇒ v }

  def map[B](f: A ⇒ B): Events[B] = Impl(events map { _ map f })

  /** Merges two event streams
    *
    * If the two streams contain events that happened at the same time
    * the events of the left (`this`) stream happened after the
    * events of the right stream
    */
  def merge[B>:A](that: Events[B]): Events[B] =
    Impl(Event.merge(events, that.events))
}

object Events extends EventsInstances {
  def apply[A](es: List[Event[A]]): Events[A] = Impl(es sortBy (_.at))

  private case class Impl[+A](events: List[Event[A]]) extends Events[A]
}

trait EventsInstances {
  implicit def EventsEqual[A:Equal]: Equal[Events[A]] =
    Equal.equalBy(_.events)

  implicit val EventsFunctor: Functor[Events] = new Functor[Events] {
    def map[A,B](e: Events[A])(f: A ⇒ B) = e map f
  }

  implicit def EventsMonoid[A]: Monoid[Events[A]] = new Monoid[Events[A]] {
    def append(a: Events[A], b: ⇒ Events[A]) = a merge b
    val zero = Events(Nil)
  }
}

// vim: set ts=2 sw=2 et:
