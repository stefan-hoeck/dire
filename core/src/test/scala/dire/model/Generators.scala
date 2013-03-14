package dire.model

import dire.Event
import org.scalacheck._, Prop._, Arbitrary.{arbitrary ⇒ arb}
import scalaz._, Scalaz._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalaCheckBinding._

trait Generators {
  implicit def EArb[A:Arbitrary]: Arbitrary[Event[A]] =
    Arbitrary(^(Gen choose (1L, 1000L), arb[A])(Event.apply))

  def nelG[A:Arbitrary]: Gen[Nel[Event[A]]] =
    arb[Nel[Event[A]]] map { es ⇒ 
      es.list sortBy (_.at) reverse match {
        case (h :: t) ⇒ nel(h, t)
        case _        ⇒ sys.error("What!?")
      }
    }

  def listG[A:Arbitrary]: Gen[List[Event[A]]] = nelG[A] map (_.tail)

  def srcG[A:Arbitrary]: Gen[Events[A]] = listG[A] map Src.apply

  implicit def eventsArb[A:Arbitrary]: Arbitrary[Events[A]] =
    Arbitrary(Gen frequency ((5, srcG[A]), (1, Never)))

  def varG[A:Arbitrary]: Gen[Signal[A]] = nelG[A] map Var.apply

  def valG[A:Arbitrary]: Gen[Signal[A]] = arb[Event[A]] map Val.apply

  implicit def signalArb[A:Arbitrary]: Arbitrary[Signal[A]] =
    Arbitrary(Gen frequency ((5, varG[A]), (1, valG[A])))
}

// vim: set ts=2 sw=2 et:
