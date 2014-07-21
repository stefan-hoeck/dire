package dire.swing

import java.awt.{GridBagLayout, GridBagConstraints, Insets}
import GridBagConstraints._
import java.awt.{Component ⇒ AComp}
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
sealed trait Elem { self ⇒ 
  import Elem.Horizontal

  def prefSize(d: Dim): Dim = identity(d)

  def adjustSize(f: Dim ⇒ Dim): Elem = new Elem {
    override def prefSize(d: Dim) = f(d)
    def height = self.height
    def width = self.width
    private[swing] def add[A:Container](x: Int, y: Int, a: A) =
      self.add(x, y, a)
  }

  def prefHeight(h: Int): Elem = adjustSize { case (w, _) ⇒ (w, h) }

  def prefWidth(w: Int): Elem = adjustSize { case (_, h) ⇒ (w, h) }

  def prefDim(w: Int, h: Int): Elem = adjustSize { _ ⇒ (w, h) }

  /** The number of rows in the grid occupied by this `Elem` */
  def height: Int

  /** The number of columns in the grid occupied by this `Elem` */
  def width: Int

  final def addTo[A](a: A)(implicit C: Container[A]): IO[A] = for {
     _ ← C.checkLayout(a)
     _ ← add(0, 0, a)
     _ ← C.adjustSize(a, prefSize)
  } yield a

  /** Tags this `Elem` to use a Monoid instance that
    * lines up elements horizontally instead of 
    * vertically (which is the default behavior)
    */
  def horizontal: Elem @@ Horizontal = Horizontal(self)

  /** Tags this `Elem` to use a Monoid instance that
    * lines up elements vertically.
    *
    * This is the default behavior so this function needs
    * only be called on `Elem`s tagged with the `Horizontal`
    * tag.
    */
  def vertical: Elem = self

  /** Creates a new panel and adds all widgets of this
    * `Elem` to it.
    */
  final def panel: IO[Panel] = panel()

  /** Creates a new panel with a border and adds all widgets of this
    * `Elem` to it.
    */
  final def panel(props: (Panel ⇒ IO[Unit])*): IO[Panel] =
    Panel(props: _*) >>= addTo[Panel]

  /** Puts an `Elem` to the right of this `Elem` */
  final def beside[A:AsElem](a: A): Elem = Elem.Beside(this, Elem(a))

  /** Puts an `Elem` below this `Elem` */
  final def above[A:AsElem](a: A): Elem = Elem.Above(this, Elem(a))

  /** Symbolic alias for `beside` */
  final def <>[A:AsElem](a: A): Elem = beside(a)

  /** Symbolic alias for `above` */
  final def ^^[A:AsElem](a: A): Elem = above(a)

  private[swing] def add[A:Container](x: Int, y: Int, a: A): IO[Unit]
}

object Elem extends AsElemInstances with AsElemSyntax {
  def apply[A:AsElem](a: A): Elem = implicitly[AsElem[A]] elem a

  private val insets = new Insets(1, 3, 1, 3)

  case class Single (
      comp: AComp,
      width: Int = 1,
      height: Int = 1,
      f: Fill = Fill.H,
      a: Anchor = Anchor.W,
      wx: Double = 1.0D,
      wy: Double = 0D,
      px: Int = 0,
      py: Int = 0,
      ins: Insets = insets) extends Elem {

    override private[swing] def add[A:Container](x: Int, y: Int, c: A) = IO {
      val cs = new GridBagConstraints(x, y, width, height, wx,
        wy, a.v, f.v, ins, px, py)

      Container[A].peer(c).add(comp, cs)
    }
  }

  val Empty = new Elem {
    val width = 0
    val height = 0

    override private[swing] def add[A:Container](x: Int, y: Int, a: A) =
      IO.ioUnit
  }

  /** This used to tag `Elem`s to use a different Monoid instance */
  sealed trait Horizontal

  /** Tags an `Elem` with the `Horizontal` tag */
  def Horizontal[A](a: A): A @@ Horizontal = Tag[A, Horizontal](a)
  
  private case class Beside (left: Elem, right: Elem) extends Elem {
    lazy val height = left.height max right.height
    lazy val width = left.width + right.width

    override private[swing] def add[A:Container](x: Int, y: Int, a: A) =
      left.add(x, y, a) >>
      right.add(x + left.width, y, a)
   
  }
  
  private case class Above (top: Elem, bottom: Elem) extends Elem {
    lazy val height = top.height + bottom.height
    lazy val width = top.width max bottom.width

    override private[swing] def add[A:Container](x: Int, y: Int, a: A) =
      top.add(x, y, a) >>
      bottom.add(x, y + top.height, a)
  }

  sealed abstract class Fill(final val v: Int) {
    import Fill._

    /** Combines two `Fill`s using the following rules:
      *
      * Combining a fill with `None` or itself returns the fill
      * Combining two distinct fills that are both not `None`
      * returns the VH fill
      */
    def plus(that: Fill): Fill = (this, that) match {
      case (None, x)         ⇒ x
      case (x, None)         ⇒ x
      case (x, y) if x == y  ⇒ x
      case _                 ⇒ VH
    }

    /** Combines two `Fill`s using the following rules:
      *
      * Subtracting a fill from itself gives `None`.
      * Subtracting H from VH gives V, V from VH gives H, and
      * None from VH gives VH. Subtracting any other fill a from
      * a fill b gives b.
      *
      * Thus it follows, that for all fills a, b
      * `a plus (b minus a) == b`
      */
    def minus(that: Fill): Fill = (this, that) match {
      case (x, y) if x == y  ⇒ None
      case (VH, H)           ⇒ V
      case (VH, V)           ⇒ H
      case (VH, None)        ⇒ VH
      case (x, _)            ⇒ x
    }
  }

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
  def elem(a: A): Elem
}

trait AsSingleElem[-A] extends AsElem[A] {
  final def elem(a: A): Elem = single(a)

  def single(a: A): Elem.Single
}

trait AsElemFunctions {
  import Elem.{Single, Fill, Anchor}

  def asElem[A](f: A ⇒ Elem): AsElem[A] = new AsElem[A] {
    def elem(a: A) = f(a)
  }

  def asSingle[A](f: A ⇒ Single): AsSingleElem[A] = new AsSingleElem[A] {
    def single(a: A) = f(a)
  }

  def noFill[A](f: A ⇒ AComp): AsSingleElem[A] = asSingle { a ⇒ 
    Single(f(a), f = Fill.None, a = Anchor.W, wx = 0D)
  }

  def hFill[A](f: A ⇒ AComp): AsSingleElem[A] = asSingle { a ⇒ Single(f(a)) }

  def vFill[A](f: A ⇒ AComp): AsSingleElem[A] = asSingle { a ⇒ 
    Single(f(a), f = Fill.V, wx = 0D, wy = 1D)
  }

  def vhFill[A](f: A ⇒ AComp): AsSingleElem[A] = asSingle { a ⇒ 
    Single(f(a), f = Fill.VH, wy = 1D)
  }
}

trait AsElemInstances0 {
  implicit val SingleAsSingle: AsSingleElem[Elem.Single] = Elem asSingle identity
}

trait AsElemInstances extends AsElemFunctions with AsElemInstances0 {
  import Elem.Horizontal

  implicit val ElemAsElem: AsElem[Elem] = asElem(identity)

  implicit val StringAsElem: AsSingleElem[String] = noFill(new JLabel(_))

  implicit val ElemMonoid = new Monoid[Elem] {
    val zero = Elem.Empty
    def append(a: Elem, b: ⇒ Elem) = a above b
  }

  implicit val HorizontalMonoid = new Monoid[Elem @@ Horizontal] {
    def zero = Elem.Empty.horizontal
    def append (a: Elem @@ Horizontal, b: ⇒ Elem @@ Horizontal) =
      a beside b horizontal
  }
}

trait AsElemSyntax {
  import Elem._

  implicit class AsElemOps[A:AsElem](val a: A) {
    def elem: Elem = implicitly[AsElem[A]] elem a

    def beside[B:AsElem](b: B): Elem = elem beside b
    def above[B:AsElem](b: B): Elem = elem above b
    def <>[B:AsElem](b: B): Elem = beside(b)
    def ^^[B:AsElem](b: B): Elem = above(b)
  }

  implicit class AsSingleElemOps[A:AsSingleElem](val a: A) {
    def single: Single = implicitly[AsSingleElem[A]] single a

    def fill(fill: Fill) = single.copy(f = fill)

    def anchor(anchor: Anchor) = single.copy(a = anchor)

    def fillH(w: Int): Single = w match {
      case 0 ⇒ single.copy(width = 1, f = single.f minus Fill.H, wx = 0.0D)
      case x ⇒ single.copy(width = x, f = single.f plus Fill.H, wx = 1.0D)
    }

    def fillV(w: Int): Single = w match {
      case 0 ⇒ single.copy(height = 1, f = single.f minus Fill.V, wy = 0.0D)
      case x ⇒ single.copy(height = x, f = single.f plus Fill.V, wy = 1.0D)
    }
  }
}

// vim: set ts=2 sw=2 et:
