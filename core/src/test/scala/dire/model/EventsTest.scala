package dire.model

import dire.Event
import org.scalacheck._, Prop._
import scalaz._, Scalaz._, scalacheck.{ScalazProperties ⇒ SP}

object EventsTest extends Properties("Event") with Generators {

  property("equal") = SP.equal.laws[Events[Int]]

  property("monoid") = SP.monoid.laws[Events[Int]]

  property("functor") = SP.functor.laws[Events]
}

// vim: set ts=2 sw=2 et:
