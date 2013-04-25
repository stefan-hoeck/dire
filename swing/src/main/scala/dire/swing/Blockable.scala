package dire.swing

import scalaz._, Scalaz._, effect.IO

trait Blockable[-A] {
  protected def isBlocked(a: A): Boolean
  protected def block(a: A, b: Boolean): Unit

  final protected def withBlock(a: A)(f: ⇒ Unit) {
    block(a, true)
    val x = f
    block(a, false)
  }

  final protected def withBlockIO(a: A)(f: IO[Unit]): IO[Unit] =
    IO(block(a, true)) >> f >> IO(block(a, false))
}

// vim: set ts=2 sw=2 et:
