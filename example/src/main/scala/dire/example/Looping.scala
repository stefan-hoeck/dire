package dire.example

import dire._, SF.EFOps, EF.{id, loop, once}
import scalaz._, Scalaz._, effect.IO

/** An event stream that uses its own output as input
  *
  * To run, modify [[dire.example.Main]] like so:
  *
  * `def runc = Looping.run`
  *
  * The application will have to be forcibly terminated
  */
object Looping {
  def run = EF.run(looping)(_ ⇒ false)

  //add two to incoming long events
  def plus2 = id[Long] mapE (1L +)

  //feedback output of the event stream to its input
  //the loop is started by the one-time event 1L
  def looping = loop(plus2 merge once(1L)).filter(_ % 100000L == 0L) --> display
  
  private def display(l: Long) = IO putStrLn l.toString
}

// vim: set ts=2 sw=2 et:
