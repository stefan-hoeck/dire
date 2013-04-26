package dire.swing

import dire._

trait IOWidget[A,B] {
  def in(a: A): SIn[B]
  def out(a: A): Sink[B]

  def sf(a: A): SF[B,B] = (SF.id to out(a)) >> in(a)
}

// vim: set ts=2 sw=2 et:
