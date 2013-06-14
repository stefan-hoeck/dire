package dire.util

import dire._
import org.scalacheck._, Prop._
import scalaz._, Scalaz._, effect.IO

object TestFunctionsTest 
  extends Properties("TestFunctions")
  with TestFunctions {

  val hundred = 0 to 100  map { _.toLong } toList

  val intId = SF.id[Int]

  val intIdH = intId hold -1

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
    simulate(is, false)(simSync) ≟ is.filter(0>)
  }

  property("simulate_awaitIni_true") = forAll { is: List[Int] ⇒
    val res = simulate(is, true)(simIniSync)
    val exp = (-1) :: is.filter(0>)

    (res ≟ exp) :| s"Exp $exp but was $res"
  }

  property("simulate_awaitIni_false_async") = forAll { is: List[Int] ⇒
    simulate(is, false)(simAsync) ≟ (is.filter(0>))
  }

  property("simulate_awaitIni_true_async") = forAll { is: List[Int] ⇒
    val res = simulate(is, true)(simIniAsync)
    val exp = (-1) :: is.filter(0>)

    (res ≟ exp) :| s"Exp $exp but was $res"
  }

  /** A signal function that holds no initial value
    *
    * Every branch confirms its completion synchronously
    */
  def simSync(o: Out[Any]): IO[SF[Int,Int]] = IO {
    intId.branch((intId filter (0 <= ) syncTo o))
         .filter(0 >)
         .syncTo(o)
  }

  /** A signal function that holds no initial value
    *
    * Every branch confirms its completion asynchronously
    */
  def simAsync(o: Out[Any]): IO[SF[Int,Int]] = IO {
    intId.branch((intId.filter(0 <= ) asyncTo o))
         .filter(0 >)
         .asyncTo(o)
  }

  /** A signal function that holds an initial value
    *
    * Every branch confirms its completion synchronously
    */
  def simIniSync(o: Out[Any]): IO[SF[Int,Int]] = IO {
    intIdH.branch((intIdH filter (0 <= ) syncTo o))
          .filter(0 >)
          .syncTo(o)
  }

  /** A signal function that holds an initial value
    *
    * Every branch confirms its completion asynchronously
    */
  def simIniAsync(o: Out[Any]): IO[SF[Int,Int]] = IO {
    intIdH.branch((intIdH filter (0 <= ) asyncTo o))
          .filter(0 >)
          .asyncTo(o)
  }
    
}

// vim: set ts=2 sw=2 et:
