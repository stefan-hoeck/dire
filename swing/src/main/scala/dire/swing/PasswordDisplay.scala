package dire.swing

import scalaz.effect.IO

trait PasswordDisplay[A] {
  def setEchoChar(a: A, c: Char): IO[Unit]
  def setPassword(a: A, s: String): IO[Unit]

  final def echoChar(a: A): Sink[Char] = sinkIO(setEchoChar(a,_))

  final def password(a: A): Sink[String] = sinkIO(setPassword(a,_))
}

// vim: set ts=2 sw=2 et:
