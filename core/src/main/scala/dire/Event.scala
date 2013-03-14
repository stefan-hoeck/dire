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

object Event extends EventInstances with EventFunctions

trait EventFunctions {

  /** Merges two lists of events.
    *
    * This function is used for merging event streams. Events will be
    * sorted in decreasing time order. If some events in both lists
    * happened at the same time, the ones in the first list will
    * be closer to the list's head (later in time).
    */
  def merge[A](as: List[Event[A]], bs: List[Event[A]]): List[Event[A]] = {
    type Es = List[Event[A]]

    val r = new collection.mutable.ListBuffer[Event[A]]

    @annotation.tailrec
    def run(xs: Es, ys: Es): Unit = (xs,ys) match {
      case (a::as, b::bs) if a.at < b.at ⇒ { r += b; run(a::as, bs)}
      case (a::as, b::bs)                ⇒ { r += a; run(as, b::bs)}
      case (as, bs)                      ⇒ { r ++= as; r ++= bs }
    }

    run(as, bs)

    r.toList
  }

  /** Zips two lists of events applying a function to all pairings.
    *
    * This function represents the Applicative combination of two
    * signals
    */
  def combine[A,B,C](as: List[Event[A]], bs: List[Event[B]])
                    (f: (A,B) ⇒ C): List[Event[C]] = {
    type Es[X] = List[Event[X]]

    val r = new collection.mutable.ListBuffer[Event[C]]

    def add(a: Event[A], b: Event[B]) { r += ^(a, b)(f) }

    @annotation.tailrec
    def run(xs: Es[A], ys: Es[B]): Unit = (xs,ys) match {
      case (a::as, b::bs) if a.at ≟ b.at ⇒ { add(a,b); run(as, bs) }
      case (a::as, b::bs) if a.at > b.at ⇒ { add(a,b); run(as, b::bs) }
      case (a::as, b::bs)                ⇒ { add(a,b); run(a::as, bs) }
      case _                             ⇒ ()
    }

    run(as, bs)

    r.toList
  }
}

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
