package dire.swing

import scalaz.effect.IO

trait Editable[-A] {
  def setEditable(a: A, b: Boolean): IO[Unit]

  final def editable(a: A): Sink[Boolean] = sinkIO(setEditable(a, _))
}

// vim: set ts=2 sw=2 et:
