package dire

import org.scalacheck._, Prop._, Arbitrary.{arbitrary ⇒ arb}
import scalaz._, Scalaz._, scalacheck.{ScalazProperties ⇒ SP}
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalaCheckBinding._

object EventTest extends Properties("Event") {

  private def once[A:Arbitrary]: Gen[Event[A]] =
    ^(Gen choose (T0, T0 + 1000), arb[A])(Once.apply)

  implicit def EArb[A:Arbitrary]: Arbitrary[Event[A]] =
    Arbitrary(Gen.frequency[Event[A]]((5, once[A]), (1, Never)))

  property("equal") = SP.equal.laws[Event[Int]]

  property("monad") = SP.monad.laws[Event]

  property("traverse") = SP.traverse.laws[Event]
}

// vim: set ts=2 sw=2 et:
