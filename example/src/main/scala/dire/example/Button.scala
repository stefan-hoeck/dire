package dire.example

import dire._, SF.EFOps
import dire.swing._
import scalaz._, Scalaz._

object ButtonApp extends SwingApp {
  def behavior = for {
    btn ← Button()
    bs  = SF.loop(btn.sf.count ∘ formatClicks)(ButtonV("0 clicks"))
    fs  = SF.seconds ∘ formatTime
  } yield (Elem(btn), bs >>> fs)

  private def formatClicks(c: Int) = ButtonV(s"$c clicks")

  private def formatTime(s: Int) = FrameV(s"Hello World (running for $s s)")
}

// vim: set ts=2 sw=2 et:
