package dire.swing

import java.awt.{Color, Font}
import javax.swing.{BorderFactory ⇒ BF}
import javax.swing.border.{Border ⇒ JBorder, BevelBorder, TitledBorder}
import scalaz._, Scalaz._, scalaz.effect.IO

sealed trait Border {
  final def apply[A:Component](a: A): IO[Unit] =
    IO(Component[A] peer a setBorder jborder)

  def jborder: JBorder
}

object Border {
  def bevel(bt: BevelType): Border = Bevel(bt, none, none)

  def bevel(bt: BevelType, h: Color, s: Color): Border =
    Bevel(bt, (h, s).some, none)

  def bevel(bt: BevelType, ho: Color, hi: Color, so: Color, si: Color): Border =
    Bevel(bt, (ho, hi).some, (so, si).some)

  val bevelLowerd: Border = bevel(BevelType.Lowered)

  val bevelRaised: Border = bevel(BevelType.Raised)

  def bevelSoft(bt: BevelType): Border = BevelSoft(bt, none, none)

  def bevelSoft(bt: BevelType, h: Color, s: Color): Border =
    BevelSoft(bt, (h, s).some, none)

  def bevelSoft(bt: BevelType, ho: Color, hi: Color, so: Color, si: Color): Border =
    BevelSoft(bt, (ho, hi).some, (so, si).some)

  val bevelSoftLowerd: Border = bevelSoft(BevelType.Lowered)

  val bevelSoftRaised: Border = bevelSoft(BevelType.Raised)

  def compound(outer: Border, inner: Border): Border = Comp(outer, inner)

  def empty: Border = Empty

  def insets(top: Int, left: Int, bottom: Int, right: Int): Border =
    Insets(top, left, bottom, right)

  def title(text: String,
            border: Option[Border] = None,
            justification: TitleJustification = TitleJustification.Default,
            position: TitlePosition = TitlePosition.Default,
            color: Option[Color] = None,
            font: Option[Font] = None): Border =
          Title(border, text, justification, position, color, font)

  implicit val BorderMonoid: Monoid[Border] = new Monoid[Border] {
    val zero = empty
    def append(a: Border, b: ⇒ Border) = compound(a, b)
  }

  /// *** Implementing classes *** ///

  private case class Bevel(
      bt: BevelType,
      i: Option[(Color,Color)],
      o: Option[(Color,Color)]
  ) extends Border {
    lazy val jborder = i cata (
      bi ⇒ o cata (
        bo ⇒ BF createBevelBorder (bt.v, bi._1, bi._2, bo._1, bo._2),
        BF createBevelBorder (bt.v, bi._1, bi._2)
      ),
      BF createBevelBorder bt.v
    )
  }

  private case class BevelSoft(
      bt: BevelType,
      i: Option[(Color,Color)],
      o: Option[(Color,Color)]
  ) extends Border {
    lazy val jborder = i cata (
      bi ⇒ o cata (
        bo ⇒ BF createSoftBevelBorder (bt.v, bi._1, bi._2, bo._1, bo._2),
        BF createSoftBevelBorder (bt.v, bi._1, bi._2)
      ),
      BF createSoftBevelBorder bt.v
    )
  }

  private case object Empty extends Border {
    val jborder = BF.createEmptyBorder
  }

  private case class Comp(o: Border, i: Border) extends Border {
    lazy val jborder = BF createCompoundBorder (o.jborder, i.jborder)
  }

  private case class Insets(t: Int, l: Int, b: Int, r: Int) extends Border {
    lazy val jborder = BF createEmptyBorder (t, l, b, r)
  }

  private case class Title(
      border: Option[Border],
      text: String,
      justification: TitleJustification,
      position: TitlePosition,
      color: Option[Color],
      font: Option[Font]
  ) extends Border {
    private def just = justification.v
    private def pos = position.v

    lazy val jborder = border cata (
      b ⇒ font cata (
        f ⇒ color cata (
          c ⇒ BF createTitledBorder(b.jborder, text, just, pos, f, c),
          BF createTitledBorder(b.jborder, text, just, pos, f)
        ),
        BF createTitledBorder(b.jborder, text, just, pos)
      ),
      BF createTitledBorder text
    )
  }
}

sealed abstract class BevelType(val v: Int)

object BevelType {
  case object Lowered extends BevelType(BevelBorder.LOWERED)
  case object Raised extends BevelType(BevelBorder.RAISED)
}

sealed abstract class TitlePosition(val v: Int)

object TitlePosition {
  case object AboveTop extends TitlePosition(TitledBorder.ABOVE_TOP)
  case object Top extends TitlePosition(TitledBorder.TOP)
  case object BelowTop extends TitlePosition(TitledBorder.BELOW_TOP)
  case object AboveBottom extends TitlePosition(TitledBorder.ABOVE_BOTTOM)
  case object Bottom extends TitlePosition(TitledBorder.BOTTOM)
  case object BelowBottom extends TitlePosition(TitledBorder.BELOW_BOTTOM)
  case object Default extends TitlePosition(TitledBorder.DEFAULT_POSITION)
}

sealed abstract class TitleJustification(val v: Int)

object TitleJustification {
  case object Left extends TitleJustification(TitledBorder.LEFT)
  case object Center extends TitleJustification(TitledBorder.CENTER)
  case object Right extends TitleJustification(TitledBorder.RIGHT)
  case object Leading extends TitleJustification(TitledBorder.LEADING)
  case object Trailing extends TitleJustification(TitledBorder.TRAILING)
  case object Default extends TitleJustification(TitledBorder.DEFAULT_JUSTIFICATION)
}

// vim: set ts=2 sw=2 et:
