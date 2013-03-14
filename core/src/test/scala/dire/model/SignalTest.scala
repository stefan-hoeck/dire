package dire.model

import dire.Event
import org.scalacheck._, Prop._
import scalaz._, Scalaz._, scalacheck.{ScalazProperties ⇒ SP}

object SignalTest extends Properties("Signal") with Generators {

  property("equal") = SP.equal.laws[Signal[Int]]

  property("applicative") = SP.applicative.laws[Signal]
}

// vim: set ts=2 sw=2 et:
