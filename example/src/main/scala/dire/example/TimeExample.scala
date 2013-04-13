package dire.example

import dire._
import scalaz._, Scalaz._, effect.IO

/** Very basic example of a signal and an event stream
  *
  * To run, modify [[dire.example.Main]] like so:
  *
  * `def runc = TimexExample.runTimeOnly` or
  * `def runc = TimexExample.runTimeFiltered`
  */
object TimeExample {
  def printTime(t: Time) = IO putStrLn s"Time is at $t microseconds"

  val stopAbove: Time = 100000L //Microseconds

  val timeOnly = SF.time --> printTime

  val timeFiltered = SF.time.filter { _ % 7L == 0L }
                            .syncTo(printTime)

  def runTimeOnly: IO[Unit] = SF.run(timeOnly)(_ >= stopAbove)

  def runTimeFiltered: IO[Unit] = SF.run(timeFiltered)(_ >= stopAbove)
}

// vim: set ts=2 sw=2 et:
