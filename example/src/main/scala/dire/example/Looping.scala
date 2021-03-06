package dire.example

import dire._, DataSink.stdOut, SF.{id, loop, const}
import scalaz._, Scalaz._, scalaz.effect.{SafeApp, IO}

/** An event stream that uses its own output as input */
object Looping extends SafeApp {

  // scalaz.effect.SafeApp entry point
  override def runc = for {
    rs   ← dire.control.ReactiveSystem()
    _    ← rs forever looping
    _    ← IO(Thread.sleep(2000))
    _    ← rs.shutdown
  } yield ()

  //add one to incoming long events and start at 1L
  def plus = id[Long] map (1L +)

  def output = id[Long] filter (_ % 10000L == 0L) to stdOut

  //feedback output of the event stream to its input
  def looping = const(1L) >=> loop(plus) >=> output
}

// vim: set ts=2 sw=2 et:
