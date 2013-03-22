package dire

import org.scalacheck._, Prop._, Arbitrary.{arbitrary ⇒ arb}
import scalaz._, Scalaz._, scalacheck.{ScalazProperties ⇒ SP}
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalaCheckBinding._

object ChangeTest extends Properties("Change") {
  implicit def EArb[A:Arbitrary]: Arbitrary[Change[A]] =
    Arbitrary(^(arb[Long], arb[A])(Change.apply))

  property("equal") = SP.equal.laws[Change[Int]]

  property("monoid") = SP.monoid.laws[Change[Int]]

  property("monad") = SP.monad.laws[Change]

  property("traverse") = SP.traverse.laws[Change]

  property("comonad") = SP.comonad.laws[Change]
}

// vim: set ts=2 sw=2 et:
