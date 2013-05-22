package dire.util

import dire._
import org.scalacheck._, Prop._
import scalaz._, Scalaz._, effect.IO

object TestFunctionsTest 
  extends Properties("TestFunctions")
  with TestFunctions {

  val hundred = 0 to 100  map { _.toLong } toList

  val intId = SF.id[Int]

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

  property("simulate_awaitIni_false") = forAll { is: List[Int] ⇒
    simulate(is, false)(sim) ≟ is.filter (0>)
  }

  property("simulate_awaitIni_true") = forAll { is: List[Int] ⇒
    simulate(is, true)(simIni) ≟ (0 :: is.filter (0>))
  }

  property("simulate_awaitIni_false_async") = forAll { is: List[Int] ⇒
    simulate(is, false)(simAsync) ≟ (is.filter (0>))
  }

  property("simulate_awaitIni_true_async") = forAll { is: List[Int] ⇒
    simulate(is, true)(simIniAsync) ≟ (0 :: is.filter (0>))
  }

  def sim(o: Out[Unit]): IO[SF[Int,Int]] = IO {
    intId branch (intId.filter(0 <= ).void syncTo o) filter (0 >)
  }

  def simIni(o: Out[Unit]): IO[SF[Int,Int]] =
    sim(o) map { _ hold 0 }

  def simAsync(o: Out[Unit]): IO[SF[Int,Int]] = IO {
    intId branch (intId.filter(0 <= ).void asyncTo o) filter (0 >)
  }

  def simIniAsync(o: Out[Unit]): IO[SF[Int,Int]] =
    simAsync(o) map { _ ⊹ SF.once(0).sf }
    
}

// vim: set ts=2 sw=2 et:
