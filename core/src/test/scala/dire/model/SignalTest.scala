package dire.model

import dire.{Event, T0, Time}
import org.scalacheck._, Prop._
import scalaz._, Scalaz._, scalacheck.{ScalazProperties ⇒ SP}

object SignalTest extends Properties("Signal") with Generators {
  implicit def SignalEqual[A:Equal]: Equal[Signal[A]] = new Equal[Signal[A]] {
    def equal(a: Signal[A], b: Signal[A]) = Times ∀ { t ⇒ a(t) ≟ b(t) }
  }

  property("equal") = SP.equal.laws[Signal[Int]]

  property("applicative") = SP.applicative.laws[Signal]

  property("time increases") = forAll { p: (Signal[Int], Signal[Int]) ⇒
    val times = ^(p._1, p._2){ _ + _ }.events map { _.at }

    times ≟ times.sorted
  }

  property("valueAt after apply") = forAll { p: (Signal[Int], Signal[Int]) ⇒
    val (sa, sb) = p
    val merged = ^(sa, sb){ _ + _ }

    def checkT(t: Time) = (merged(t), sa(t), sb(t)) match {
      case (c, a, b) if c ≟ (a + b) ⇒ true
      case (c, a, b) ⇒ { println(s"$c, ${a + b}, at time $t"); false }
    }

    Times ∀ checkT
  }
}

// vim: set ts=2 sw=2 et:
