package dire.example

import dire._
import dire.swing._, Swing._
import scalaz._, Scalaz._

object TextFields extends SwingApp {

  override def behavior(f: Frame) = for {
    first ← TextField(columns := 50)
    last  ← TextField()
    full  ← Label()

    //Layout (available through class Elem and its implicit syntax classes)
    _ ← ("First name" beside first) above
        ("Last name" beside last) above
        ("Full name" beside full) addTo f

    //Behavior:
    //Concatenate values of signals first and last and output to full
    sf  = ^(first.textIn, last.textIn){ _ + " " + _ } to full.text
  } yield sf

}

// vim: set ts=2 sw=2 et:
