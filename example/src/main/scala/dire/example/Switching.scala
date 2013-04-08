package dire.example

import dire._, SF.EFOps
import scalaz._, Scalaz._, effect.IO

object Switching {
  def ticks(i: Int) = (EF ticks ((i + 1) * 100000L)).count

  def switch = (SF.seconds switch ticks) --> { i ⇒ IO putStrLn i.toString }

  def run: IO[Unit] = SF.run(switch.ef.count)(_ >= 100000)
}

// vim: set ts=2 sw=2 et:
