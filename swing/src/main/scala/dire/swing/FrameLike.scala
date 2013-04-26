package dire.swing

import java.awt.{Frame ⇒ AFrame}

trait FrameLike[A] extends Window[A] {
  def peer(a: A): AFrame

  final def maximizedBounds(a: A): Sink[Rect] = 
    sink(b ⇒ peer(a).setMaximizedBounds(rectangle(b)))

  final def resizable(a: A): Sink[Boolean] = sink(peer(a).setResizable)

  final def title(a: A): Sink[String] = sink(peer(a).setTitle)

  final def undecorated(a: A): Sink[Boolean] = sink(peer(a).setUndecorated)
}

object FrameLike {
  def apply[A:FrameLike]: FrameLike[A] = implicitly

  implicit val FrameLikeIso = new (FrameLike ~>> AFrame) {
    def apply[A](f: FrameLike[A], a: A) = f peer a
  }
}

// vim: set ts=2 sw=2 et:
