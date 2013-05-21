package dire.util

import dire._, DataSink.{buffer, stdOut}, SF.{id, loop, const, never}
import dire.control.Var
import scala.collection.mutable.{ListBuffer ⇒ MList}
import scalaz._, Scalaz._, effect.IO
import scalaz.concurrent.Strategy

/** Provides a couple of helper functions to test reactive
  * behaviors.
  *
  * All tests run with a fixed sized thread pool of only two threads per
  * default. Override `poolSize` to change this.
  *
  * For convenience, all methods in this trait return their values
  * not wrapped in `IO` though they perform side effects.
  */
trait TestFunctions {
  val Sequential: Option[Strategy] = Some(Strategy.Sequential)

  /** Number of threads in fixed sized thread pool */
  protected val poolSize = 2

  /** Time steps in micro seconds */
  protected val step = 1L

  /** Collects events of an input signal function but uses another
    * signal function for the abbort condition
    */
  def run[A,B](in: SIn[A])(ab: SF[A,B])(stop: B ⇒ Boolean): List[A] = {
    val as = new MList[A]
    val coll = in to buffer(as) andThen ab

    SF.run(coll, poolSize, step = step)(stop).unsafePerformIO

    as.toList
  }

  /** Collects events of an input signal for t microseconds */
  def runFor[A](in: SIn[A], t: Time): List[A] = 
    run(in)(SF.time.sf[A])(t <= _)

  /** Collects n events of an input signal */
  def runN[A](in: SIn[A], n: Int): List[A] = 
    run(in)(SF.id[A].count)(n <= _)

  /** Collects events of an input signal until an event is fired
    * that fullfills the given predicate
    */
  def runUntil[A](in: SIn[A])(stop: A ⇒ Boolean): List[A] =
    run(in)(SF.id[A])(stop)

  //@TODO: More documentation
  /** Simulates a reactive setup that depends on some mutable state */
  def simulate[E,I](events: List[E])
                   (sf: Out[Unit] ⇒ IO[SF[E,I]]): List[I] = {
      val es = events.toIndexedSeq
      type Or = Unit \/ I

      def total(sf: SF[E,I], v: Var[Unit]): SIn[Or] = {
        val eventSF = id[Or].count map es andThen sf.events

        loop(v.in.sf[Or].events or eventSF).in
      }

      def totalIO = for {
        v ← Var newVar ()
        s ← sf(v.put)
      } yield total(s, v)

      runN(SF io totalIO, es.size) collect { case \/-(i) ⇒ i }
    }
}

object test extends TestFunctions

// vim: set ts=2 sw=2 et:
