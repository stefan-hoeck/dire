package dire.example

import dire._, SF._
import dire.swing._
import scalaz._, Scalaz._

object ButtonApp extends SwingApp {
  def behavior = for {
    btn ← Button()
    bs  = loop(btn.sf.count ∘ formatClicks)(ButtonV("0 clicks"))
    fs  = time(oneSec) ∘ { t ⇒ formatTime( t / oneSec) } 
  } yield (Elem(btn), bs >>> fs)

  private def formatClicks(c: Int) = ButtonV(s"$c clicks")

  private def formatTime(t: Time) = FrameV(s"Hello World (running for $t s)")

  val oneSec = 1000000L
}

// vim: set ts=2 sw=2 et:
