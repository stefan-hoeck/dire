package dire.control

import dire._
import org.scalacheck._, Prop._
import scalaz._, Scalaz._, effect.IO

trait Runner {

  private val o100: List[Change[Time]] =
    0 to 100 map { i ⇒ Change(i, i.toLong) } toList

  def test100[A,O[+_]:IdOrEvent]
    (sf: RF[Time,A,Id,O])
    (f: Time ⇒ Option[O[A]])
    (implicit E: Equal[O[A]]): Boolean = {
    val changes = runFor(sf, 100L)

    (o100 flatMap { c ⇒ f(c.v) map (Change(c.at,_)) }) ≟ changes
  }
  /** Builds a reactive graph and runs it until an event is fired
    * that fullfills the given predicate
    */
  def runUntil[A,O[+_]:IdOrEvent]
    (in: RF[⊥,A,Event,O])
    (stop: A ⇒ Boolean): List[Change[O[A]]] = {
    val as = new collection.mutable.ListBuffer[Change[O[A]]]
    val coll = in changeTo (a ⇒ IO(as += a))

    SF.run(coll, 4, step = 1L)(stop).unsafePerformIO

    as.toList
  }

  /** Builds a reactive graph and runs it for t microseconds
    * while collecting change events of type A.
    */
  def runFor[A,O[+_]:IdOrEvent](sf: RF[Time,A,Id,O], t: Time)
    : List[Change[O[A]]] = {
    val as = new collection.mutable.ListBuffer[Change[O[A]]]
    val time = RF.time branch sf.changeTo(a ⇒ IO(as += a))

    SF.run(time, 4, step = 1L)(t <= _).unsafePerformIO

    as.toList
  }

  /** Compares the events fired by two signal functions
    */
  def compare[A,B,O1[+_]:IdOrEvent,O2[+_]:IdOrEvent]
    (sfA: RF[Time,A,Id,O1], sfB: RF[Time,B,Id,O2], t: Time)
    (exp: List[Change[O1[A]]] ⇒ List[Change[O2[B]]])
    (implicit E: Equal[O2[B]]): Boolean = {
    val as = new collection.mutable.ListBuffer[Change[O1[A]]]
    val bs = new collection.mutable.ListBuffer[Change[O2[B]]]
    val time = SF.time
                  .branch(sfA.changeTo(a ⇒ IO(as += a)))
                  .branch(sfB.changeTo(b ⇒ IO(bs += b)))

    SF.run(time, 4, step = 1L)(t <= _).unsafePerformIO

    //println(as1.toList take 3)

    val should = exp(as.toList)
    val was = bs.toList

    val res = should ≟ was

    if (! res) {
      val shouldStr = should mkString "\n"
      val wasStr = was mkString "\n"

      println(s"Exp:\n$shouldStr\nWas:$wasStr")
    }

    res
  }
}

// vim: set ts=2 sw=2 et:
