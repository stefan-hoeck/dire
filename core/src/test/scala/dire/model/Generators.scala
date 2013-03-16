package dire.model

import dire.{Event, Time, T0}
import org.scalacheck._, Prop._, Arbitrary.{arbitrary ⇒ arb}
import scalaz._, Scalaz._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalaCheckBinding._

trait Generators {
  //Do not go much higher since for testing signal-equality we must
  //iterate over all possible values of `Time`
  final val TMax = T0 + 100L

  final val Times: List[Time] = Enum[Time].fromToL(T0, TMax)

  implicit def EArb[A:Arbitrary]: Arbitrary[Event[A]] =
    Arbitrary(^(Gen choose (T0, TMax), arb[A])(Event.apply))

  def listG[A:Arbitrary]: Gen[List[Event[A]]] = Gen listOf arb[Event[A]]

  def mapG[A:Arbitrary]: Gen[Map[Time,A]] = 
    listG[A] map { _ map { e ⇒ (e.at, e.v) } toMap }

  implicit def eventsArb[A:Arbitrary]: Arbitrary[Events[A]] =
    Arbitrary(listG[A] map Events.apply)

  implicit def signalArb[A:Arbitrary]: Arbitrary[Signal[A]] =
    Arbitrary(^(arb[A], mapG[A])(Signal.apply))
}

// vim: set ts=2 sw=2 et:
