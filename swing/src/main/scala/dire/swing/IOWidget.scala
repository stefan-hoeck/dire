package dire.swing

import dire._
import scala.reflect.runtime.universe.TypeTag

trait IOWidget[A,B] {
  def cachedIn(a: A)(implicit T: TypeTag[B]): SIn[B] =
    SF cached (in(a), a)

  def cachedSf(a: A)(implicit T: TypeTag[B]): SF[B,B] =
    SF cached (sf(a), a)

  def in(a: A): SIn[B]

  def out(a: A): Sink[B]

  def sf(a: A): SF[B,B] = (SF.id to out(a)) >> in(a)
}

// vim: set ts=2 sw=2 et:
