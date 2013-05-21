package dire.util

import dire._
import org.scalacheck._, Prop._
import scalaz._, Scalaz._

object TestFunctionsTest 
  extends Properties("TestFunctions")
  with TestFunctions {

  val hundred = 0 to 100  map { _.toLong } toList

  property("run") = 
    run(SF.time)(SF sf { _.toString })("100" ≟ _) ≟ hundred

  property("runFor") = runFor(SF.time, 100L) ≟ hundred

  property("runN") = runN(SF.time, 101) ≟ hundred

  property("runUntil") = runUntil(SF.time)(100L <= _) ≟ hundred

  property("runFor_const") = forAll { i: Int ⇒ 
    runFor(SF const i, 100L) ≟ List(i)
  }

  property("runN_const") = forAll { i: Int ⇒ 
    runN(SF const i, 1) ≟ List(i)
  }

  property("runUntil_const") = forAll { i: Int ⇒ 
    runUntil(SF const i)(i ≟ _) ≟ List(i)
  }
}

// vim: set ts=2 sw=2 et:
