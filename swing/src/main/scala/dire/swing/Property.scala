package dire.swing

import dire.Out
import java.awt.{Font, Color, Component ⇒ AComponent, 
                 Window ⇒ AWindow, Image}
import javax.swing._
import javax.swing.border.Border
import scalaz.effect.IO

trait Property[A,F[_]] {
  def := [B](a: A)(b: B)(implicit Witness: F[B]): IO[Unit]
}

object Property {
  def apply[A,F[_],R](f: (A,R) ⇒ Unit)(implicit X: (F ~>> R)): Property[A,F] =
    io[A,F,R]((a,r) ⇒ IO(f(a,r)))

  def io[A,F[_],R](f: (A,R) ⇒ IO[Unit])(implicit X: (F ~>> R))
    : Property[A,F] = new Property[A,F] {
      def := [B](a: A)(b: B)(implicit W: F[B]): IO[Unit] = f(a,X[B](W, b))
    }

  private[dire] def compP[A](f: AComponent ⇒ A ⇒ Unit) =
    apply[A,Comp,AComponent]((a,c) ⇒ f(c)(a))

  private[dire] def componentP[A](f: JComponent ⇒ A ⇒ Unit) =
    apply[A,Component,JComponent]((a,c) ⇒ f(c)(a))

  private[dire] def windowP[A](f: AWindow ⇒ A ⇒ Unit) =
    apply[A,Window,AWindow]((a,c) ⇒ f(c)(a))
}

trait Properties {
  import Property._

  val background = compP(_.setBackground)

  val border = componentP(_.setBorder)

  val bounds = compP[Rect](c ⇒ r ⇒ c.setBounds(rectangle(r)))

  val cursor = compP(_.setCursor)

  val disabledIcon = new Property[Icon,IconDisplay] {
    def := [B](a: Icon)(b: B)(implicit W: IconDisplay[B]) =
      W.setDisabledIcon(b, a)
  }

  val doubleBuffered = componentP(_.setDoubleBuffered)

  val enabled = compP(_.setEnabled)

  val focusable = compP(_.setFocusable)

  val font = compP(_.setFont)

  val foreground = compP(_.setForeground)

  val hAlign = new Property[HAlign,TextDisplay] {
    def := [B](a: HAlign)(b: B)(implicit W: TextDisplay[B]) = W.setHAlign(b, a)
  }

  val hTextPos = new Property[HAlign,TextDisplay] {
    def := [B](a: HAlign)(b: B)(implicit W: TextDisplay[B]) = W.setHTextPos(b, a)
  }

  val icon = new Property[Icon,IconDisplay] {
    def := [B](a: Icon)(b: B)(implicit W: IconDisplay[B]) = W.setIcon(b, a)
  }

  val iconImage = windowP(_.setIconImage)

  val iconImages = {
    import scala.collection.JavaConversions._

    windowP[List[Image]](c ⇒ is ⇒ c.setIconImages(is))
  }

  val iconTextGap = new Property[Int,IconDisplay] {
    def := [B](a: Int)(b: B)(implicit W: IconDisplay[B]) =
      W.setIconTextGap(b, a)
  }

  val maxSize = compP[Dim](c ⇒ d ⇒ c.setMaximumSize(dimension(d)))

  val minSize = compP[Dim](c ⇒ d ⇒ c.setMinimumSize(dimension(d)))

  val name = compP(_.setName)

  val opaque = componentP(_.setOpaque)

  val preferredSize = compP[Dim](c ⇒ d ⇒ c.setPreferredSize(dimension(d)))

  val size = compP[Dim](c ⇒ d ⇒ c.setSize(dimension(d)))

  val text = new Property[String,TextDisplay] {
    def := [B](a: String)(b: B)(implicit W: TextDisplay[B]) = W.setText(b, a)
  }

  val tooltip =
    componentP[Option[String]](c ⇒ os ⇒ c.setToolTipText(os getOrElse null))

  val vAlign = new Property[VAlign,TextDisplay] {
    def := [B](a: VAlign)(b: B)(implicit W: TextDisplay[B]) = W.setVAlign(b, a)
  }

  val visible = compP(_.setVisible)

  val vTextPos = new Property[VAlign,TextDisplay] {
    def := [B](a: VAlign)(b: B)(implicit W: TextDisplay[B]) = W.setVTextPos(b, a)
  }
}

/** This trait is used to conveniently define properties with
  * a minimum of boilerplate.
  */
trait ~>>[F[_],R] {
  def apply[A](f: F[A], a: A): R
}

// vim: set ts=2 sw=2 et: