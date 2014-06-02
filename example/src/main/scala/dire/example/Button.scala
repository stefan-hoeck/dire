package dire.example

import dire._
import dire.swing._, Swing._
import scalaz._, Scalaz._

/** A simple user inteface with a button that counts its own clicks */
object ButtonApp extends SwingApp {
  override def behavior(f: Frame) = for {
    btn ← Button(text := "0 clicks")

    //Layout (available through class Elem and its implicit syntax classes)
    _   ← "Click me" beside btn addTo f

    //Behavior:
    //Count number of button clicks and output to button's actual text >>>
    //Keep track of time for which application ran and output to frame's title
    sf  = (btn.clicks.count map clicksStr to btn.text) >>
          (SF.seconds map timeStr to f.title)
  } yield sf

  private def clicksStr(c: Int) = s"$c clicks"

  private def timeStr(s: Int) = s"Hello World (running for $s s)"
}

// vim: set ts=2 sw=2 et:
