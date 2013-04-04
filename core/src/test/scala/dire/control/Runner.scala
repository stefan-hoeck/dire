package dire.control

import dire.{SF, SIn, Time, Change}
import org.scalacheck._, Prop._
import scalaz._, Scalaz._, effect.IO

trait Runner {

  /** Builds a reactive graph and runs it until an event is fired
    * that fullfills the given predicate
    */
  def runUntil[A](in: SIn[A], stop: A ⇒ Boolean): List[Change[A]] = {
    val as = new collection.mutable.ListBuffer[Change[A]]
    val coll = in changeTo (a ⇒ IO(as += a))

    SF.run(coll, step = 1L)(stop).unsafePerformIO

    as.toList
  }

  /** Builds a reactive graph and runs it for t microseconds
    * while collecting change events of type A.
    */
  def runFor[A](sf: SF[Time,A], t: Time): List[Change[A]] = {
    val as = new collection.mutable.ListBuffer[Change[A]]
    val time = SF.time to sf.changeTo(a ⇒ IO(as += a))

    SF.run(time, step = 1L)(t <= _).unsafePerformIO

    as.toList
  }

  /** Compares the events fired by two signal functions
    */
  def compare[A,B:Equal]
    (sfA: SF[Time,A], sfB: SF[Time,B], t: Time)
    (exp: List[Change[A]] ⇒ List[Change[B]]): Boolean = {
    val as = new collection.mutable.ListBuffer[Change[A]]
    val bs = new collection.mutable.ListBuffer[Change[B]]
    val time = SF.time
                 .to(sfA.changeTo(a ⇒ IO(as += a)))
                 .to(sfB.changeTo(b ⇒ IO(bs += b)))

    SF.run(time, step = 1L)(t <= _).unsafePerformIO

    //println(as1.toList take 3)

    val should = exp(as.toList)
    val was = bs.toList

    val res = should ≟ was

    if (! res) println(s"Exp: $should, but was: $was")

    res
  }
}

// vim: set ts=2 sw=2 et:
