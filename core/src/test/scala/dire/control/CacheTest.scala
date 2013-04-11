package dire.control

import org.scalacheck._, Prop._
import scalaz._, Scalaz._, effect.IO

object CacheTest extends Properties("Cache") {

  property("addAndGet") = forAll { s: String ⇒ 
    val c = new Cache
    val first = c(IO(s), s).unsafePerformIO
    val second = c(IO(""), s).unsafePerformIO

    first ≟ second
  }

  property("addAndGet_differentKey") = forAll { s: String ⇒ 
    val c = new Cache
    val first = c(IO(s), s).unsafePerformIO
    val second = c(IO(""), s + "b").unsafePerformIO

    (first ≟ s) && (second ≟ "")
  }

  property("addAndGet_supertype") = forAll { s: String ⇒ 
    val c = new Cache
    val first: String = c(IO(s), s).unsafePerformIO
    val second: Any = c(IO("": Any), s).unsafePerformIO

    first == second
  }

  property("addAndGet_subtype") = forAll { s: String ⇒ 
    val c = new Cache
    val first: Any = c(IO(s: Any), s).unsafePerformIO
    val second: String = c(IO(s + "a"), s).unsafePerformIO

    (first == s) && (second ≟ (s + "a"))
  }
}

// vim: set ts=2 sw=2 et:
