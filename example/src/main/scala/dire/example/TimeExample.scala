package dire.example

import dire.{SF, Time, T0, Event}, SF.EventsOps
import scalaz._, Scalaz._, effect.IO

object TimeExample {
  def printTime(t: Time) = IO putStrLn s"Time is at $t microseconds"

  val stopAbove: Time = 100000L //Microseconds

  val timeOnly = SF.time(1000L) --> printTime

  val timeFiltered = SF.time(1000L).changes
                                   .filter { _ % 7L == 0L }
                                   .eventTo(printTime)

  def runTimeOnly: IO[Unit] = SF.runS(timeOnly)(_ >= stopAbove)

  def runTimeFiltered: IO[Unit] = SF.runE(timeFiltered)(_ >= stopAbove)
}

// vim: set ts=2 sw=2 et:
