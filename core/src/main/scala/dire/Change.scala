package dire

import scalaz._, Scalaz._

/** Represents a change in a signal at a certain time
  *
  * @param at   the time at which the event happened
  * @param v    the value of the event
  */
final case class Change[+A](at: Time, v: A) {
  def map[B](f: A ⇒ B): Change[B] = Change(at, f(v))

  def flatMap[B](f: A ⇒ Change[B]): Change[B] = f(v) match {
    case Change(atB, b) ⇒ Change(at max atB, b)
  }
}

object Change extends ChangeInstances with ChangeFunctions {

  /** The most general function needed to calculate the
    * latest change of a signal from two input signals.
    *
    * All primitive functions that create a new signal from
    * one or two input signals that is synchronously updated
    * can be expressed using this type and type 'InitialS'.
    */
  type NextS[-A,-B,C] = (Change[A], Change[B], Change[C]) ⇒ Change[C]

  /** The most general function needed to calculate the
    * initial value of a signal from two input signals.
    *
    * All primitive functions that create a new signal from
    * one or two input signals that is synchronously updated
    * can be expressed using this type and type 'NextS'.
    */
  type InitialS[-A,-B,C] = (Change[A], Change[B]) ⇒ Change[C]
}

trait ChangeInstances {
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
trait ChangeFunctions {
  import Change.{ChangeMonoid, ChangeMonad, ChangeEqual, NextS, InitialS}

  // All functions here are pure so they can be properly tested.

  def applyI[A,B,C](f: (A,B) ⇒ C): InitialS[A,B,C] =
    (ca,cb) ⇒ ^(ca,cb)(f)

  def applyN[A,B,C](f: (A,B) ⇒ C): NextS[A,B,C] =
    (ca,cb,_) ⇒ ^(ca,cb)(f)

  def changesI[A:Equal]: InitialS[A,Any,Event[A]] = (ca,_) ⇒ ca as Never

  def changesN[A:Equal]: NextS[A,Any,Event[A]] = {
    case (Change(_,a1),_, c2@Change(_,Once(a2))) if a1 ≟ a2 ⇒ c2
    case (ca,_,_) ⇒ ca map Once.apply
  }

  def collectI[A,B](f: A ⇒ Option[B]): InitialS[Event[A],Any,Event[B]] =
    (cea,_) ⇒ cea map { _ collect f }

  def collectN[A,B](f: A ⇒ Option[B]): NextS[Event[A],Any,Event[B]] = {
    case (Change(t, ea),_,ceb) ⇒ 
      ea collect f fold (b ⇒ Change(t, Once(b)), ceb)
  }

  def mergeI[A]: InitialS[Event[A],Event[A],Event[A]] = (c1,c2) ⇒ 
    later(c1, c2) | ^(c1, c2)(_ orElse _)

  def mergeN[A]: NextS[Event[A],Event[A],Event[A]] = (c1,c2,c3) ⇒ 
    later(c1, c2) | ^(c1, c2)(_ orElse _) match {
      case Change(_, Never) ⇒ c3
      case x                ⇒ x
    }

  private def later[A](c1: Change[A], c2: Change[A]) =
    if (c1.at > c2.at) Some(c1)
    else if (c2.at > c1.at) Some(c2)
    else None

  def mapI[A,B](f: A ⇒ B): InitialS[A,Any,B] = (ca,_) ⇒ ca map f

  def mapN[A,B](f: A ⇒ B): NextS[A,Any,B] = (ca,_,cc) ⇒ ca map f

  def scanI[A,B](ini: ⇒ B)(f: (A,B) ⇒ B): InitialS[Event[A],Any,B] = {
    case (Change(t, Once(a)), _) ⇒ Change(t, f(a, ini))
    case (Change(t, Never), _)   ⇒ Change(t, ini)
  }

  def scanN[A,B](ini: ⇒ B)(f: (A,B) ⇒ B): NextS[Event[A],Any,B] = {
    case (Change(t, Once(a)), _, Change(t2, b)) ⇒ Change(t max t2, f(a, b))
    case (_, _, cb)                             ⇒ cb
  }
}

// vim: set ts=2 sw=2 et:
