package dire.swing

import java.awt.{Graphics, Color, Graphics2D, Toolkit}
import javax.swing.JPanel
import scalaz._, Scalaz._, effect.IO

object Animation {
  
  class Scene private(c: Color) {
    private[this] var actual: Shape = Blank

    private[dire] def display(s: Shape): IO[Unit] = IO {
      actual = s
      comp.repaint()
    }

    private final object comp extends JPanel {
      setBackground(c)

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
    def apply(c: Color = Color.BLACK): IO[Scene] = IO(new Scene(c))

    implicit val SceneAsElem: AsElem[Scene] = Elem vhFill { _.comp }

    implicit val SceneSink: Sink[Scene,Shape] = sink(_.display)
  }

  type Point = (Double, Double)

  sealed trait Shape {
    private[swing] def paint(g: Graphics2D)
  }

  case object Blank extends Shape {
    private[swing] def paint(g: Graphics2D) {}
  }

  case class Circle(center: Point, r: Int, color: Color) extends Shape {
    private[swing] def paint(g: Graphics2D) {
      g.setColor(color)
      g.fillOval(center._1.toInt, center._2.toInt, r, r)
    }
  }

  case class Combo(a: Shape, b: Shape) extends Shape {
    private[swing] def paint(g: Graphics2D) {
      a.paint(g)
      b.paint(g)
    }
  }

  object Shape {
    implicit val ShapeMonoid: Monoid[Shape] = new Monoid[Shape] {
      val zero = Blank
      def append(a: Shape, b: ⇒ Shape): Shape = Combo(a, b)
    }
  }
}

// vim: set ts=2 sw=2 et:
