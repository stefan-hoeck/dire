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
  def behavior(f: Frame) = for {
    btn ← Button()
    sf  = (btn.clicks.count map clicksStr toE btn.text) >>>
          (SF.seconds map timeStr toE f.title)
  } yield (Elem(btn) setDim (200, 100), sf)

  private def clicksStr(c: Int) = s"$c clicks"

  private def timeStr(s: Int) = s"Hello World (running for $s s)"
}

// vim: set ts=2 sw=2 et:
