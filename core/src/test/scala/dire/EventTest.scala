package dire

import org.scalacheck._, Prop._, Arbitrary.{arbitrary ⇒ arb}
import scalaz._, Scalaz._, scalacheck.{ScalazProperties ⇒ SP}
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalaCheckBinding._

object EventTest extends Properties("Event") {
  import Event.{once, never}

  private def onceG[A:Arbitrary]: Gen[Event[A]] =
    ^(Gen choose (T0, T0 + 1000), arb[A])(once(_,_))

  implicit def EArb[A:Arbitrary]: Arbitrary[Event[A]] =
    Arbitrary(Gen.frequency[Event[A]]((5, onceG[A]), (1, never[A].point[Gen])))

  include(SP.equal.laws[Event[Int]])

  include(SP.monad.laws[Event])

  include(SP.traverse.laws[Event])
}

// vim: set ts=2 sw=2 et:
