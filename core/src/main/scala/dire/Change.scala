package dire

import scalaz._, Scalaz._

/** Represents a change in a signal at a certain time
  *
  * @param at   the time at which the event happened
  * @param v    the value of the event
  */
final private[dire] case class Change[+A](at: Time, v: A) {
  def map[B](f: A ⇒ B): Change[B] = Change(at, f(v))

  def flatMap[B](f: A ⇒ Change[B]): Change[B] = f(v) match {
    case Change(atB, b) ⇒ Change(at max atB, b)
  }
}

private[dire] object Change extends ChangeInstances with ChangeFunctions

private[dire] trait ChangeInstances {
  implicit def ChangeEqual[A:Equal]: Equal[Change[A]] =
    Equal.equalBy(e ⇒ (e.at, e.v))

  implicit val ChangeMonad: Monad[Change] with
                           Comonad[Change] with
                           Traverse[Change] =
    new Monad[Change] with Comonad[Change] with Traverse[Change]{
      def point[A](a: ⇒ A) = Change(Long.MinValue, a)
      def bind[A,B](e: Change[A])(f: A ⇒ Change[B]) = e flatMap f
      def cobind[A,B](e: Change[A])(f: Change[A] ⇒ B) = Change(e.at, f(e))
      def cojoin[A](e: Change[A]) = Change(e.at, e)
      def copoint[A](e: Change[A]) = e.v
      def traverseImpl[G[_]:Applicative,A,B](e: Change[A])(f: A ⇒ G[B])
        : G[Change[B]] = f(e.v) map { Change(e.at, _ ) }
    }

  implicit def ChangeMonoid[A:Monoid]: Monoid[Change[A]] =
    Monoid.liftMonoid[Change,A]
}

/** Lots of helper functions to create new signals from one or two
  * input signals.
  *
  * These functions are used to implement most of the functions defined
  * on [[dire.Signal]]. See there for proper documentation.
  */
private[dire] trait ChangeFunctions {
  import Change.{ChangeMonoid, ChangeMonad, ChangeEqual}
//
//  def mergeI[A]: InitialS[Event[A],Event[A],Event[A]] = (c1,c2) ⇒ 
//    later(c1, c2) | ^(c1, c2)(_ orElse _)
//
//  def mergeN[A]: NextS[Event[A],Event[A],Event[A]] = (c1,c2,c3) ⇒ 
//    later(c1, c2) | ^(c1, c2)(_ orElse _) match {
//      case Change(_, Never) ⇒ c3
//      case x                ⇒ x
//    }
//
//  private def later[A](c1: Change[A], c2: Change[A]) =
//    if (c1.at > c2.at) Some(c1)
//    else if (c2.at > c1.at) Some(c2)
//    else None
}

// vim: set ts=2 sw=2 et:
