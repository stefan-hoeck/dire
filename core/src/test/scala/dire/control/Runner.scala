package dire.control

import dire._
import org.scalacheck._, Prop._
import scala.collection.mutable.{ListBuffer ⇒ MList}
import scalaz._, Scalaz._, effect.IO

trait Runner {
  import Runner.Events

  private val ps = 2
  private val runs = 100
  private val runsL: Long = runs

  private val o100: List[Time] = 0 to runs map { _.toLong } toList

  def test100[A:Equal](sf: SF[Time,A])(f: List[Time] ⇒ List[A]): Boolean = {
    val changes = runFor(sf, runsL)

    val res = f(o100) ≟ changes.flatMap { _.toOption }
    if (!res) println(changes mkString "\n")
    res
  }

  def test100O[A:Equal](sf: SF[Time,A])(f: Time ⇒ Option[A]): Boolean =
    test100(sf)(_ map f flatten )

  /** Builds a reactive graph and runs it until an event is fired
    * that fullfills the given predicate
    */
  def runUntil[A](in: SIn[A])(stop: A ⇒ Boolean): Events[A] = {
    val as = new MList[Event[A]]
    val coll = in eventsTo (a ⇒ IO(as += a))

    SF.run(coll, ps, step = 1L)(stop).unsafePerformIO

    as.toList
  }

  /** Builds a reactive graph and runs it for t microseconds
    * while collecting change events of type A.
    */
  def runFor[A](sf: SF[Time,A], t: Time): Events[A] = {
    val as = new MList[Event[A]]
    val time = SF.time branch sf.eventsTo(a ⇒ IO(as += a))

    SF.run(time, ps, step = 1L)(t <= _).unsafePerformIO

    as.toList
  }

  /** Compares the events fired by two signal functions
    */
  def compare[A,B:Equal]
    (sfA: SF[Time,A], sfB: SF[Time,B], t: Time)
    (exp: Events[A] ⇒ Events[B]): Boolean = {
    val as = new MList[Event[A]]
    val bs = new MList[Event[B]]
    val time = SF.time
                 .branch(sfA.eventsTo(a ⇒ IO(as += a)))
                 .branch(sfB.eventsTo(b ⇒ IO(bs += b)))

    SF.run(time, ps, step = 1L)(t <= _).unsafePerformIO

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

object Runner {
  type Events[+A] = List[Event[A]]
}

// vim: set ts=2 sw=2 et:
