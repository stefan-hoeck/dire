package dire.swing

import javax.swing.SwingConstants._

sealed abstract class Alignment(val v: Int)

object Alignment {
  case object Left extends Alignment(LEFT)
  case object Right extends Alignment(RIGHT)
  case object Center extends Alignment(CENTER)
  case object Top extends Alignment(TOP)
  case object Bottom extends Alignment(BOTTOM)

  case object Leading extends Alignment(LEADING)
  case object Trailing extends Alignment(TRAILING)
}

// vim: set ts=2 sw=2 et:
