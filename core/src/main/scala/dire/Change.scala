package dire

import scalaz._, Scalaz._

/** Represents a change in a signal at a certain time
  *
  * @param at   the time at which the event happened
  * @param v    the value of the event
  */
//final private[dire] case class Change[+A](at: Time, v: A) {
//  def map[B](f: A ⇒ B): Change[B] = Change(at, f(v))
//
//  def flatMap[B](f: A ⇒ Change[B]): Change[B] = f(v) match {
//    case Change(atB, b) ⇒ Change(at max atB, b)
//  }
//}
//
//private[dire] object Change extends ChangeInstances
//
//private[dire] trait ChangeInstances {
//  implicit def ChangeEqual[A:Equal]: Equal[Change[A]] =
//    Equal.equalBy(e ⇒ (e.at, e.v))
//
//  implicit val ChangeMonad: Monad[Change] with
//                           Comonad[Change] with
//                           Traverse[Change] =
//    new Monad[Change] with Comonad[Change] with Traverse[Change]{
//      def point[A](a: ⇒ A) = Change(Long.MinValue, a)
//      def bind[A,B](e: Change[A])(f: A ⇒ Change[B]) = e flatMap f
//      def cobind[A,B](e: Change[A])(f: Change[A] ⇒ B) = Change(e.at, f(e))
//      def cojoin[A](e: Change[A]) = Change(e.at, e)
//      def copoint[A](e: Change[A]) = e.v
//      def traverseImpl[G[_]:Applicative,A,B](e: Change[A])(f: A ⇒ G[B])
//        : G[Change[B]] = f(e.v) map { Change(e.at, _ ) }
//    }
//
//  implicit def ChangeMonoid[A:Monoid]: Monoid[Change[A]] =
//    Monoid.liftMonoid[Change,A]
//}

// vim: set ts=2 sw=2 et:
