package dire.example

import dire._, SF.{id, loop, sfIO, const} 
import scalaz._, Scalaz._, effect.IO

object HelloWorld {

  private final val Quit = ":q"

  private def prompt(s: String) =
    IO.putStrLn(s"Hello ${s}!") >>
    IO.putStrLn(s"Please enter your name ($Quit to exit).") >>
    IO.readLn

  def quit = id filter Quit.≟

  def name = id filter Quit.≠

  def hello = loop(name andThen sfIO(prompt))

  def goodbye = name on quit syncTo { s ⇒ IO putStrLn s"Goodbye $s!" }

  def run = SF.run(const("World") >=> hello >=> goodbye)(_ ⇒ true)
}

// vim: set ts=2 sw=2 et:
