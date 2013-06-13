package dire.control

import dire._
import org.scalacheck._, Prop._
import scalaz._, Scalaz._, effect.IO

object ReactiveSystemTest extends Properties("ReactiveSystem") {
  property("shutdown") = {
    val res = for {
      rs ← ReactiveSystem()
      _  ← rs forever (SF const 0)
      _  ← rs.shutdown
    } yield true

    res.unsafePerformIO()
  }
}

// vim: set ts=2 sw=2 et:
