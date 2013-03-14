package dire

import org.scalacheck._, Prop._, Arbitrary.{arbitrary ⇒ arb}
import scalaz._, Scalaz._, scalacheck.{ScalazProperties ⇒ SP}
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalaCheckBinding._

object EventTest extends Properties("Event") {
  implicit def EArb[A:Arbitrary]: Arbitrary[Event[A]] =
    Arbitrary(^(arb[Long], arb[A])(Event.apply))

  property("equal") = SP.equal.laws[Event[Int]]

  property("monoid") = SP.monoid.laws[Event[Int]]

  property("monad") = SP.monad.laws[Event]

  property("traverse") = SP.traverse.laws[Event]

  property("comonad") = SP.comonad.laws[Event]
}

// vim: set ts=2 sw=2 et:
