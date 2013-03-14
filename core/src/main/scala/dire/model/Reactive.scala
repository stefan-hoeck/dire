package dire.model

import dire.{Time, Event}
import scalaz._, Scalaz._

sealed trait Reactive[+A] {
  /** List of events in decreasing time order */
  def events: List[Event[A]]
}

sealed trait Signal[+A] extends Reactive[A] {
  def es: NonEmptyList[Event[A]]
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
}

/** A changing signal */
case class Var[+A](es: NonEmptyList[Event[A]]) extends Signal[A]

/** A constant signal */
case class Val[+A](a: Event[A]) extends Signal[A] {
  def es = NonEmptyList(a)
}

sealed trait Events[+A] extends Reactive[A] {
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
    * whatsoever
    */ 
  def at(t: Time): List[A] = events collect { case Event(`t`,v) ⇒ v }

  def map[B](f: A ⇒ B): Events[B] = Src(events map { _ map f })

  /** Merges two event streams
    *
    * If the two streams contain events that happened at the same time
    * the events of the left (`this`) stream happened later
    */
  def merge[B>:A](that: Events[B]): Events[B] = {
    @annotation.tailrec
    def run(as: List[Event[B]],
            bs: List[Event[B]],
            acc: List[Event[B]]): List[Event[B]] = (as, bs) match {
      case (a::ar, b::br) if (a.at >= b.at) ⇒ run(a :: ar, br, b :: acc)
      case (a::ar, b::br)                   ⇒ run(ar, b :: br, a :: acc)
      case (ar, br)                         ⇒ ar ::: br ::: acc
    }

    Src(run(this.events, that.events, Nil))
  }
}

/** An source of events */
case class Src[+A](events: List[Event[A]]) extends Events[A]

/** The empty event stream */
case object Never extends Events[Nothing] {
  def events = Nil
}

// vim: set ts=2 sw=2 et:
