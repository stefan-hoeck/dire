package dire.example

import dire._, SF.EFOps
import scalaz._, Scalaz._, effect.IO

object TimeExample {
  def printTime(t: Time) = IO putStrLn s"Time is at $t microseconds"

  val stopAbove: Time = 100000L //Microseconds

  val timeOnly = SF.time --> printTime

  val timeFiltered = SF.time.changes
                            .filter { _ % 7L == 0L }
                            .syncTo(printTime)

  def runTimeOnly: IO[Unit] = SF.run(timeOnly)(_ >= stopAbove)

  def runTimeFiltered: IO[Unit] = EF.run(timeFiltered)(_ >= stopAbove)
}

// vim: set ts=2 sw=2 et:
