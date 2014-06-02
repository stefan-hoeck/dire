package dire.example

import dire._
import scalaz._, Scalaz._, scalaz.effect.{SafeApp, IO}

/** Very basic example of a signal and an event stream */
object TimeExample extends SafeApp {
  def printTime(t: Time) = IO putStrLn s"Time is at $t microseconds"

  val stopAbove: Time = 100000L //Microseconds

  val timeOnly = SF.time --> printTime

  val timeFiltered = SF.time.filter { _ % 7L == 0L }
                            .syncTo(printTime)

  def runTimeOnly: IO[Unit] = SF.run(timeOnly)(_ >= stopAbove)

  def runTimeFiltered: IO[Unit] = SF.run(timeFiltered)(_ >= stopAbove)

  // scalaz.effect.SafeApp entry point
  override def runc = runTimeOnly /* or runTimeFiltered */
}

// vim: set ts=2 sw=2 et:
