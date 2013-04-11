package dire

import scalaz._, Scalaz._

/** An abstract class to represent event occurences in
  * an event stream.
  *
  * Clients are not supposed to access or manipulate objects of this
  * type directly but should use the corresponding combinators
  * defined for signal functions instead
  */
sealed trait Event[+A] extends Any {
  private[dire] def at: Time

  private[dire] def orAt(t: Time): Event[A] =
    fold(Once(at max t, _), Never)

  private[dire] def fold[B](once: A ⇒ B, never: ⇒ B): B = this match {
    case Never     ⇒ never
    case Once(_,v) ⇒ once(v)
  }

  private[dire] def flatMap[B](f: A ⇒ Event[B]): Event[B] = 
    fold[Event[B]](f(_) orAt at, Never)

  private[dire] def map[B](f: A ⇒ B): Event[B] =
    flatMap(a ⇒ Once(at, f(a)))

  private[dire] def collect[B](f: A ⇒ Option[B]): Event[B] =
    flatMap(a ⇒ f(a).fold[Event[B]](Never)(Once(at,_)))

  private[dire] def orElse[B>:A](e: ⇒ Event[B]): Event[B] =
    fold(Once(at, _), e)

  private[dire] def toOption: Option[A] = fold(Some(_), None)
}

private[dire] case object Never extends Event[Nothing] {
  val at = T0
}

private[dire] case class Once[+A](at: Time, v: A) extends Event[A]

object Event {
  def apply[A](o: Option[A]): Event[A] =
    o.fold[Event[A]](Never)(Once(T0,_))

  implicit def EventEqual[A:Equal]: Equal[Event[A]] = new Equal[Event[A]] {
    def equal(a: Event[A], b: Event[A]) = (a,b) match {
      case (Once(at1, a),Once(at2, b)) ⇒ (at1 ≟ at2) && (a ≟ b)
      case (Never,Never)               ⇒ true
      case _                           ⇒ false
    }
  }

  implicit val EventMonad: Monad[Event] with Traverse[Event] =
    new Monad[Event] with Traverse[Event] {
      def point[A](a: ⇒ A) = Once(T0, a)
      def bind[A,B](e: Event[A])(f: A ⇒ Event[B]) = e flatMap f
      def traverseImpl[G[_],A,B](fa: Event[A])
                                (f: A ⇒ G[B])
                                (implicit F: Applicative[G]): G[Event[B]] =
      fa.fold(a ⇒ F.map(f(a))(Once(fa.at, _)), F point Never)
    }
}

// vim: set ts=2 sw=2 et:
