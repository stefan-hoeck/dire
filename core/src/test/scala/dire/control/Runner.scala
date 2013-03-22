package dire.control

import dire.{SF, Time, Change}
import org.scalacheck._, Prop._
import scalaz._, Scalaz._, effect.IO

trait Runner {
  /** Builds a reactive graph and runs it for t microseconds
    * while collecting change events of type A.
    */
  def runFor[A](sf: SF[Time,A], t: Time): List[Change[A]] = {
    val as = new collection.mutable.ListBuffer[Change[A]]
    val time = SF.time branch sf.changeTo(a ⇒ IO(as += a))

    SF.runReactive(time, 1L)(t <= _).unsafePerformIO

    as.toList
  }

  /** Two signal functions are equal if in the same reactive run
    * they produce the same events at the same times.
    *
    * Note: Since signal functions may contain asynchronous
    * components, the cannot be compared across different runs.
    * Strictly synchronous SF can be compared across different
    * runs.
    */
  def compare[A:Equal](sf1: SF[Time,A], t: Time)(sf2: SF[Time,A])
    : Boolean = {
    val as1 = new collection.mutable.ListBuffer[Change[A]]
    val as2 = new collection.mutable.ListBuffer[Change[A]]
    val time = SF.time
                 .branch(sf1.changeTo(a ⇒ IO(as1 += a)))
                 .branch(sf2.changeTo(a ⇒ IO(as2 += a)))

    SF.runReactive(time, 1L)(t <= _).unsafePerformIO

    //println(as1.toList take 3)

    (as1.toList ≟ as2.toList)
  }
}

// vim: set ts=2 sw=2 et:
