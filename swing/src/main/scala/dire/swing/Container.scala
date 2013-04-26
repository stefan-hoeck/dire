package dire.swing

import java.awt.{Container ⇒ AContainer, GridBagLayout}
import scalaz.effect.IO

/** Type class representing a `java.awt.Container` */
trait Container[-A] extends Comp[A] {
  def peer(a: A): AContainer

  final private[swing] def checkLayout(a: A): IO[Unit] = IO {
    if (! peer(a).getLayout.isInstanceOf[GridBagLayout])
      peer(a).setLayout(new GridBagLayout)
  }

  def adjustSize(a: A, f: Dim ⇒ Dim): IO[Unit] = IO {
    val old = peer(a).getPreferredSize
    peer(a).setPreferredSize(dimension(f(old.width, old.height)))
  }
}

object Container {
  def apply[A:Container]: Container[A] = implicitly

  implicit val ContainerIso = new (Container ~>> AContainer) {
    def apply[A](f: Container[A], a: A) = f peer a
  }
}

// vim: set ts=2 sw=2 et:
