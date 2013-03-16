package dire.model

import dire.Event
import org.scalacheck._, Prop._
import scalaz._, Scalaz._, scalacheck.{ScalazProperties ⇒ SP}

object EventsTest extends Properties("Event") with Generators {

  property("equal") = SP.equal.laws[Events[Int]]

  property("monoid") = SP.monoid.laws[Events[Int]]

  property("functor") = SP.functor.laws[Events]

  property("merge size") = forAll { p: (Events[Int], Events[Int]) ⇒
    val (as, bs) = p
    (as merge bs).events.size ≟ (as.events.size + bs.events.size)
  }

  property("same events") = forAll { p: (Events[Int], Events[Int]) ⇒
    val (as, bs) = p
    (as merge bs).events.toSet == (as.events ++ bs.events).toSet
  }

  property("time increases") = forAll { p: (Events[Int], Events[Int]) ⇒
    val (as, bs) = p
    def times[A](es: List[Event[A]]) = es map { _.at }

    times((as merge bs).events) ≟ times(as.events ++ bs.events).sorted
  }

  property("left preference") = forAll { p: (Events[Int], Events[Int]) ⇒
    val (as, bs) = p
    val all = (as merge bs).events
    //groups events of both event streams by time
    val timeToEvents = bs.events.groupBy(_.at) ⊹ as.events.groupBy(_.at)

    timeToEvents.toList ∀ { all containsSlice _._2 }
  }
}

// vim: set ts=2 sw=2 et:
