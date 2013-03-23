package dire

/** An abstract class to represent event occurences in
  * an event stream.
  *
  * Clients are not supposed to access or manipulate objects of this
  * class directly but should use the corresponding combinators
  * defined for signal functions instead
  */
sealed trait Event[+A] extends Any {
  private[dire] def fold[B](once: A ⇒ B, never: ⇒ B): B =
    this match {
      case Never ⇒ never
      case Once(v) ⇒ once(v)
    }

  private[dire] def flatMap[B](f: A ⇒ Event[B]): Event[B] = 
    fold[Event[B]](f, Never)

  private[dire] def map[B](f: A ⇒ B): Event[B] = flatMap(a ⇒ Once(f(a)))

  private[dire] def filter(p: A ⇒ Boolean): Event[A] =
    flatMap(a ⇒ if (p(a)) Once(a) else Never)

  private[dire] def collect[B](f: A ⇒ Option[B]): Event[B] =
    flatMap(a ⇒ f(a).fold[Event[B]](Never)(Once.apply))

  private[dire] def orElse[B>:A](e: ⇒ Event[B]): Event[B] =
    fold(Once(_), e)
}

private[dire] case object Never extends Event[Nothing]

private[dire] case class Once[+A](v: A) extends Event[A]

object Event {
  //do NOT provide an implementation for type class Equal here. This guarantees,
  //that function 'changes' in 'SF' can only be called on non-events
}

// vim: set ts=2 sw=2 et:
