package dire.example

import dire._, SF.EFOps
import dire.swing._
import scalaz._, Scalaz._

/** A simple user inteface with a button that counts its own clicks
  *
  * To run, modify [[dire.example.Main]] like so:
  *
  * `def runc = ButtonApp.run`
  */
object ButtonApp extends SwingApp {
  def behavior = for {
    btn ← Button()
    bs  = SF.loop(btn.sf.count ∘ formatClicks)(ButtonV("0 clicks"))
    fs  = SF.seconds ∘ formatTime
  } yield (Elem(btn) setDim (200, 100), bs >>> fs)

  private def formatClicks(c: Int) = ButtonV(s"$c clicks")

  private def formatTime(s: Int) = FrameV(s"Hello World (running for $s s)")
}

// vim: set ts=2 sw=2 et:
