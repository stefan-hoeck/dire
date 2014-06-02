package dire.example

import dire._
import dire.swing._, Swing._
import scalaz._, Scalaz._, effect.IO

/** A user interface with a group of four radio buttons */
object ButtonGroup extends SwingApp {
  override def behavior(f: Frame) = for {
    lbl  ← Label(text := " ")
    btns ← labels traverse (l ⇒ RadioButton(text := l))

    //Layout (available through class Elem and its implicit syntax classes)
    _   ← btns foldMap (Elem(_)) above lbl addTo f

    //Group radio buttons and display actually selected value in lbl
    sf  = SF.io(btns zip labels group) to lbl.text in
  } yield sf

  val labels = List("Apple", "Orange", "Banana", "Pear", "Hotdog")
}

// vim: set ts=2 sw=2 et:
