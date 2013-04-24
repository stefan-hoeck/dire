package dire.swing

import javax.swing.Icon
import scalaz.effect.IO

trait IconDisplay[-A] {
  def setDisabledIcon(a: A, i: Icon): IO[Unit]
  def setIcon(a: A, i: Icon): IO[Unit]
  def setIconTextGap(a: A, i: Int): IO[Unit]

  final def disabledIcon(a: A): Sink[Icon] = sinkIO(setDisabledIcon(a,_))

  final def icon(a: A): Sink[Icon] = sinkIO(setIcon(a,_))

  final def iconTextGap(a: A): Sink[Int] = sinkIO(setIconTextGap(a,_))
}

// vim: set ts=2 sw=2 et:
