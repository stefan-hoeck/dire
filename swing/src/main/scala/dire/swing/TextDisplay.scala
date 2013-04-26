package dire.swing

import javax.swing.SwingConstants._
import scalaz.Show
import scalaz._, Scalaz._
import scalaz.effect.IO

/** Type class representing a widget that can display text */
trait TextDisplay[-A] extends Blockable[A] {
  def setText(a: A, s: String): IO[Unit]

  final def text(a: A): Sink[String] = sinkIO{ t ⇒ 
    withBlockIO(a){ setText(a, t) }
  }

  final def textA[B](a: A): Sink[B] = text(a) contramap { _.toString }

  final def textS[B:Show](a: A): Sink[B] = text(a) contramap Show[B].shows
}

// vim: set ts=2 sw=2 et:
