package dire

import org.scalacheck._, Prop._, Arbitrary.arbitrary
import scalaz._, Scalaz._, scalacheck.{ScalazProperties ⇒ SP}

object EventTest extends Properties("Event") {
  implicit def arb[A:Arbitrary]: Arbitrary[Event[A]] = Arbitrary(
    for { t ← arbitrary[Long]; v ← arbitrary[A] } yield Event(t, v)
  )

  property("equal") = SP.equal.laws[Event[Int]]

  property("monoid") = SP.monoid.laws[Event[Int]]

  property("monad") = SP.monad.laws[Event]

  property("traverse") = SP.traverse.laws[Event]

  property("comonad") = SP.comonad.laws[Event]
}

// vim: set ts=2 sw=2 et:
