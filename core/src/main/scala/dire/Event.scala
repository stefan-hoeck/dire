package dire

import scalaz._, Scalaz._

/** Represents an event that happened at a certain time
  * in a reactive system
  *
  * @param at   the time at which the event happened
  * @param v    the value of the event
  */
final case class Event[+A](at: Time, v: A) {
  def happenedAt(t: Time): Boolean = at ≟ t

  def map[B](f: A ⇒ B): Event[B] = Event(at, f(v))

  def flatMap[B](f: A ⇒ Event[B]): Event[B] = f(v) match {
    case Event(atB, b) ⇒ Event(at max atB, b)
  }
}

object Event extends EventInstances

trait EventInstances {
  implicit def EventEqual[A:Equal]: Equal[Event[A]] =
    Equal.equalBy(e ⇒ (e.at, e.v))

  implicit val EventMonad: Monad[Event] with
                           Comonad[Event] with
                           Traverse[Event] =
    new Monad[Event] with Comonad[Event] with Traverse[Event]{
      def point[A](a: ⇒ A) = Event(Long.MinValue, a)
      def bind[A,B](e: Event[A])(f: A ⇒ Event[B]) = e flatMap f
      def cobind[A,B](e: Event[A])(f: Event[A] ⇒ B) = Event(e.at, f(e))
      def cojoin[A](e: Event[A]) = Event(e.at, e)
      def copoint[A](e: Event[A]) = e.v
      def traverseImpl[G[_]:Applicative,A,B](e: Event[A])(f: A ⇒ G[B])
        : G[Event[B]] = f(e.v) map { Event(e.at, _ ) }
    }

  implicit def EventMonoid[A:Monoid]: Monoid[Event[A]] =
    Monoid.liftMonoid[Event,A]
}

// vim: set ts=2 sw=2 et:
