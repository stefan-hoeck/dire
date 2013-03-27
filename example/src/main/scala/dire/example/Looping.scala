package dire.example

import dire._, SF.EventsOps
import scalaz._, Scalaz._, effect.IO

object Looping {
  def run = SF.runE(looping)(100000L <= _)

  //add two to incoming long events
  def plus2 = Arrow[SF].id[Event[Long]] mapE (2L+)

  //feedback output of the event stream to its input
  //the loop is started by the one-time event 1L
  def looping = SF.loopE(plus2 merge SF.once(1L)) --?> display
  
  private def display(l: Long) = IO putStrLn l.toString
}

// vim: set ts=2 sw=2 et:
