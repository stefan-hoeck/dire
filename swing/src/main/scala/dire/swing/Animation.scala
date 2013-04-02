package dire.swing

import java.awt.{Graphics, Color, Graphics2D, Toolkit}
import javax.swing.JPanel
import scalaz._, Scalaz._, effect.IO

object Animation {
  
  class Scene private(c: Color) {
    private[this] var actual: Shape = blank

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

  lazy val blank: Shape = new Shape {
    private[swing] def paint(g: Graphics2D) {}
  }

  def circle(x: Double, y: Double, r: Double, c: Color): Shape =
    oval(x, y, r, r, c)

  def rectangle(x: Double, y: Double, rx: Double, ry: Double, c: Color)
    : Shape = new Shape {
      private[swing] def paint(g: Graphics2D) {
        g.setColor(c)
        g.fillRect(x.toInt, y.toInt, rx.toInt, ry.toInt)
      }
    }

  def oval(x: Double, y: Double, rx: Double, ry: Double, c: Color): Shape =
    new Shape {
      private[swing] def paint(g: Graphics2D) {
        g.setColor(c)
        g.fillOval(x.toInt, y.toInt, rx.toInt, ry.toInt)
      }
    }

  def square(x: Double, y: Double, s: Double, c: Color) = 
    rectangle(x, y, s, s, c)

  def combine(a: Shape, b: Shape): Shape  = new Shape {
    private[swing] def paint(g: Graphics2D) {
      a.paint(g)
      b.paint(g)
    }
  }

  object Shape {
    implicit val ShapeMonoid: Monoid[Shape] = new Monoid[Shape] {
      val zero = blank
      def append(a: Shape, b: ⇒ Shape): Shape = combine(a, b)
    }
  }
}

// vim: set ts=2 sw=2 et:
