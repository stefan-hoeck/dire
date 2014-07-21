package dire.swing

import java.awt.{Graphics, Graphics2D, Toolkit}
import javax.swing.JPanel
import scalaz._, Scalaz._, effect.IO

final class Scene private () {
  private[this] var actual: Shape = Shape.blank

  def display: Sink[Shape] =
    sink(s ⇒ { actual = s; peer.repaint() })

  final object peer extends JPanel {
    setDoubleBuffered(true)

    override protected def paintComponent(g: Graphics) {
      super.paintComponent(g)

      g match {
        case g: Graphics2D ⇒ {
          actual.paint(g)
          Toolkit.getDefaultToolkit.sync()
          g.dispose()
        }
        case _             ⇒ ()
      }
    }
  }
}

object Scene {
  def apply(props: (Scene ⇒ IO[Unit])*): IO[Scene] = for {
    res ← IO(new Scene())
    _   ← props.toList foldMap { _(res) }
  } yield res

  implicit val SceneComponent: Component[Scene] = new Component[Scene] {
    def peer(p: Scene) = p.peer
  }

  implicit val SceneAsElem: AsSingleElem[Scene] = Elem vhFill { _.peer }
}

// vim: set ts=2 sw=2 et:
