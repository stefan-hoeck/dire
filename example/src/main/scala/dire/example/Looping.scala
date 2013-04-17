package dire.example

import dire._, SF.{id, loop, once}
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
  def run = for {
    kill ← SF.forever(looping)
    _    ← IO(Thread.sleep(4000))
    _    ← kill
  } yield ()

  //add one to incoming long events and start at 1L
  def plus = id[Long] map (1L +) hold 1L

  //feedback output of the event stream to its input
  def looping = loop(plus) filter (_ % 10000L == 0L) syncTo display
  
  private def display(l: Long) = IO putStrLn l.toString
}

// vim: set ts=2 sw=2 et:
