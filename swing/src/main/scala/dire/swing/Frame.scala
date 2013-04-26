package dire.swing

import dire._
import java.awt.{BorderLayout, Color, Dimension}
import java.awt.event.{WindowListener, WindowEvent}
import javax.swing.JFrame
import scalaz._, Scalaz._, effect.IO

class Frame(val peer: JFrame)

object Frame {
  def apply(props: Frame ⇒ IO[Unit]*): IO[Frame] = for {
    res ← IO(new Frame(new JFrame()))
    _   ← props.toList foldMap { _(res) }
  } yield res

  implicit val FrameComponent = new FrameLike[Frame] {
    def peer(b: Frame) = b.peer
  }
}

// vim: set ts=2 sw=2 et:
