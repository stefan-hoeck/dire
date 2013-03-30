package dire.swing

import java.awt.{GridBagLayout, GridBagConstraints, Insets}
import GridBagConstraints._
import javax.swing.{JComponent ⇒ JComp, JLabel}
import scalaz._, Scalaz._, effect.IO

/** Used to describe a typical UI-Layout
  *
  * An `Elem` is either a single widget in a user interface
  * (represented by class [[dire.swing.Elem.Single]]) or
  * a combination of several such widgets. Elements can
  * be combined using the `beside` and `above` combinators.
  * For single elements it is possible to more explicitly
  * define their behavior in terms of where they are anchored
  * whether they should try to fill out one or both dimensions
  * and so on.
  *
  * Elements are based on the `java.awt.GridBagLayout` but offer
  * a much nicer syntax to arrange complex user interfaces
  */
sealed trait Elem {
  def height: Int
  def width: Int

  final def addToPanel(p: Panel): IO[Panel] = add(0, 0, p) as p

  final def panel: IO[Panel] = Panel() >>= addToPanel

  final def beside(e: Elem): Elem = Elem.Beside(this, e)
  final def above(e: Elem): Elem = Elem.Above(this, e)
  final def <>(e: Elem): Elem = beside(e)
  final def ^^(e: Elem): Elem = above(e)

  private[swing] def add(x: Int, y: Int, p: Panel): IO[Unit]
}

object Elem extends AsElemInstances with AsElemSyntax {
  def apply[A:AsElem](a: A): Elem = implicitly[AsElem[A]] elem a

  private val insets = new Insets(1, 3, 1, 3)

  case class Single (
      comp: JComp,
      width: Int = 1,
      height: Int = 1,
      f: Fill = Fill.H,
      a: Anchor = Anchor.NW,
      wx: Double = 1.0D,
      wy: Double = 0D,
      px: Int = 0,
      py: Int = 0,
      ins: Insets = insets) extends Elem {

    override private[swing] def add(x: Int, y: Int, p: Panel) = IO {
      val cs = new GridBagConstraints(x, y, width, height, wx,
        wy, a.v, f.v, ins, px, py)

      p.peer.add(comp, cs)
    }
  }

  val Empty = new Elem {
    val width = 0
    val height = 0
    override private[swing] def add(x: Int, y: Int, p: Panel) = IO.ioUnit
  }
  
  private case class Beside (left: Elem, right: Elem) extends Elem {
    lazy val height = left.height max right.height
    lazy val width = left.width + right.width

    override private[swing] def add(x: Int, y: Int, p: Panel)  = 
      left.add(x, y, p) >>
      right.add(x + left.width, y, p)
   
  }
  
  private case class Above (top: Elem, bottom: Elem) extends Elem {
    lazy val height = top.height + bottom.height
    lazy val width = top.width max bottom.width

    override private[swing] def add(x: Int, y: Int, p: Panel) =
      top.add(x, y, p) >>
      bottom.add(x, y + top.height, p)
  }

  sealed abstract class Fill(final val v: Int)

  object Fill {
    case object None extends Fill(NONE)
    case object V extends Fill(VERTICAL)
    case object H extends Fill(HORIZONTAL)
    case object VH extends Fill(BOTH)
  }

  sealed abstract class Anchor(final val v: Int)

  object Anchor {
    case object N extends Anchor(NORTH)
    case object NE extends Anchor(NORTHEAST)
    case object E extends Anchor(EAST)
    case object SE extends Anchor(SOUTHEAST)
    case object S extends Anchor(SOUTH)
    case object SW extends Anchor(SOUTHWEST)
    case object W extends Anchor(WEST)
    case object NW extends Anchor(NORTHWEST)
    case object Center extends Anchor(CENTER)
  }
}

trait AsElem[-A] {
  def elem(a: A): Elem.Single
}

trait AsElemFunctions {
  import Elem.{Single, Fill, Anchor}

  def asElem[A](f: A ⇒ Single): AsElem[A] = new AsElem[A] {
    def elem(a: A) = f(a)
  }

  def noFill[A](f: A ⇒ JComp): AsElem[A] = asElem { a ⇒ 
    Single(f(a), f = Fill.None, a = Anchor.W, wx = 0D)
  }

  def hFill[A](f: A ⇒ JComp): AsElem[A] = asElem { a ⇒ Single(f(a)) }

  def vFill[A](f: A ⇒ JComp): AsElem[A] = asElem { a ⇒ 
    Single(f(a), f = Fill.V, wx = 0D, wy = 1D)
  }

  def vhFill[A](f: A ⇒ JComp): AsElem[A] = asElem { a ⇒ 
    Single(f(a), f = Fill.VH, wy = 1D)
  }
}

trait AsElemInstances extends AsElemFunctions {
  implicit val StringAsElem: AsElem[String] = noFill(new JLabel(_))

  implicit val ElemAsElem: AsElem[Elem.Single] = asElem(identity)
}

trait AsElemSyntax {
  import Elem._

  implicit class AsElemOps[A:AsElem](val a: A) {
    def elem: Single = implicitly[AsElem[A]] elem a

    def fill(fill: Fill) = elem.copy(f = fill)

    def anchor(anchor: Anchor) = elem.copy(a = anchor)

    def fillH(w: Int): Single = {
      val newFill = elem.f match {
        case Fill.V  ⇒ Fill.VH
        case Fill.VH ⇒ Fill.VH
        case _       ⇒ Fill.H
      }

      elem.copy(width = w, f = newFill, wx = 1.0D)
    }

    def fillV(h: Int): Single = {
      val newFill = elem.f match {
        case Fill.H  ⇒ Fill.VH
        case Fill.VH ⇒ Fill.VH
        case _       ⇒ Fill.V
      }

      elem.copy(height = h, f = newFill, wy = 1.0D)
    }
  }
}

// vim: set ts=2 sw=2 et:
