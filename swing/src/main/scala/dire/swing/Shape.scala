package dire.swing

import java.awt.{Graphics, Color, Graphics2D, Toolkit}
import scala.collection.immutable.{IndexedSeq ⇒ IxSeq}
import scalaz._, Scalaz._

trait Shape {
  def paint(g: Graphics2D): Unit
}

object Shape {
  lazy val blank: Shape = new Shape {
    def paint(g: Graphics2D) {}
  }

  def circle(x: Double, y: Double, r: Double, c: Color): Shape =
    oval(x, y, r, r, c)

  def polyLine(points: IxSeq[Position], c: Color): Shape = new Shape {
    val xs = points map { _._1 } toArray
    val ys = points map { _._2 } toArray
    val size = points.size

    def paint(g: Graphics2D) {
      g.setColor(c)

      g.drawPolyline(xs, ys, size)
    }
  }

  def rectangle(x: Double, y: Double, rx: Double, ry: Double, c: Color)
    : Shape = new Shape {
      def paint(g: Graphics2D) {
        g.setColor(c)
        g.fillRect(x.toInt, y.toInt, rx.toInt, ry.toInt)
      }
    }

  def oval(x: Double, y: Double, rx: Double, ry: Double, c: Color): Shape =
    new Shape {
      def paint(g: Graphics2D) {
        g.setColor(c)
        g.fillOval(x.toInt, y.toInt, rx.toInt, ry.toInt)
      }
    }

  def square(x: Double, y: Double, s: Double, c: Color) = 
    rectangle(x, y, s, s, c)

  def combine(a: Shape, b: Shape): Shape  = new Shape {
    def paint(g: Graphics2D) {
      a.paint(g)
      b.paint(g)
    }
  }

  implicit val ShapeMonoid: Monoid[Shape] = new Monoid[Shape] {
    val zero = blank
    def append(a: Shape, b: ⇒ Shape): Shape = combine(a, b)
  }
}

// vim: set ts=2 sw=2 et:
