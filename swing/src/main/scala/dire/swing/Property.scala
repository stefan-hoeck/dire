package dire.swing

import dire.Out
import java.awt.{Font, Color, Component ⇒ AComponent, 
                 Window ⇒ AWindow, Image, Frame ⇒ AFrame}
import javax.swing._, javax.swing.{AbstractButton ⇒ JAbstractButton}
import javax.swing.text.JTextComponent
import scalaz.Foldable
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

  private[dire] def abstractButtonP[A](f: JAbstractButton ⇒ A ⇒ Unit) =
    apply[A,AbstractButton,JAbstractButton]((a,c) ⇒ f(c)(a))

  private[dire] def comboBoxP[A,B](f: JComboBox[B] ⇒ A ⇒ Unit) =
    apply[A,({type λ[α]=ComboBoxLike[α,B]})#λ, JComboBox[B]]((a,c) ⇒ f(c)(a))

  private[dire] def compP[A](f: AComponent ⇒ A ⇒ Unit) =
    apply[A,Comp,AComponent]((a,c) ⇒ f(c)(a))

  private[dire] def componentP[A](f: JComponent ⇒ A ⇒ Unit) =
    apply[A,Component,JComponent]((a,c) ⇒ f(c)(a))

  private[dire] def frameLikeP[A](f: AFrame ⇒ A ⇒ Unit) =
    apply[A,FrameLike,AFrame]((a,c) ⇒ f(c)(a))

  private[dire] def scrollPaneP[A](f: JScrollPane ⇒ A ⇒ Unit) =
    apply[A,ScrollPaneLike,JScrollPane]((a,c) ⇒ f(c)(a))

  private[dire] def textCompP[A](f: JTextComponent ⇒ A ⇒ Unit) =
    apply[A,TextComponent,JTextComponent]((a,c) ⇒ f(c)(a))

  private[dire] def textFieldLikeP[A](f: JTextField ⇒ A ⇒ Unit) =
    apply[A,TextFieldLike,JTextField]((a,c) ⇒ f(c)(a))

  private[dire] def windowP[A](f: AWindow ⇒ A ⇒ Unit) =
    apply[A,Window,AWindow]((a,c) ⇒ f(c)(a))
}

trait Properties {
  import Property._

  val background = compP(_.setBackground)

  val border = componentP[Border](c ⇒ b ⇒ c.setBorder(b.jborder))

  val bounds = compP[Rect](c ⇒ r ⇒ c.setBounds(rectangle(r)))

  val caret = textCompP(_.setCaret)

  val caretColor = textCompP(_.setCaretColor)

  val caretPosition = textCompP(_.setCaretPosition)

  val cursor = compP(_.setCursor)

  val columns = textFieldLikeP(_.setColumns)

  val disabledIcon = new Property[Icon,IconDisplay] {
    def := [B](a: Icon)(b: B)(implicit W: IconDisplay[B]) =
      W.setDisabledIcon(b, a)
  }

  val disabledTextColor = textCompP(_.setDisabledTextColor)

  val doubleBuffered = componentP(_.setDoubleBuffered)

  val echoChar = new Property[Char,PasswordDisplay] {
    def := [B](a: Char)(b: B)(implicit W: PasswordDisplay[B]) =
      W.setEchoChar(b, a)
  }

  val editable = new Property[Boolean,Editable] {
    def := [B](a: Boolean)(b: B)(implicit W: Editable[B]) =
      W.setEditable(b, a)
  }

  val enabled = compP(_.setEnabled)

  val focusable = compP(_.setFocusable)

  val font = compP(_.setFont)

  val foreground = compP(_.setForeground)

  val hAlign = new Property[HAlign,TextAlign] {
    def := [B](a: HAlign)(b: B)(implicit W: TextAlign[B]) = W.setHAlign(b, a)
  }

  val hScrollBarPolicy = scrollPaneP { p ⇒ h: HScrollBarPolicy ⇒ 
    p.setHorizontalScrollBarPolicy(h.v) }

  val hTextPos = new Property[HAlign,TextAlign] {
    def := [B](a: HAlign)(b: B)(implicit W: TextAlign[B]) = W.setHTextPos(b, a)
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

  def item[A] = comboBoxP[A,A](_.setSelectedItem)

  def items[A,F[_]:Foldable] = new Property[F[A],({type λ[α]=ComboBoxLike[α,A]})#λ] {
    def := [B](as: F[A])(b: B)(implicit W: ComboBoxLike[B,A]) = W.setItems(b, as)
  }

  val maximizedBounds =
    frameLikeP[Rect](c ⇒ d ⇒ c.setMaximizedBounds(rectangle(d)))

  val maxSize = compP[Dim](c ⇒ d ⇒ c.setMaximumSize(dimension(d)))

  val minSize = compP[Dim](c ⇒ d ⇒ c.setMinimumSize(dimension(d)))

  val name = compP(_.setName)

  val opaque = componentP(_.setOpaque)

  val password = new Property[String,PasswordDisplay] {
    def := [B](s: String)(b: B)(implicit W: PasswordDisplay[B]) =
      W.setPassword(b, s)
  }

  val preferredSize = compP[Dim](c ⇒ d ⇒ c.setPreferredSize(dimension(d)))

  val resizable = frameLikeP(_.setResizable)

  val selected = abstractButtonP(_.setSelected)

  val selectedTextColor = textCompP(_.setSelectedTextColor)

  val selectionColor = textCompP(_.setSelectionColor)

  val selectionEnd = textCompP(_.setSelectionEnd)

  val selectionStart = textCompP(_.setSelectionStart)

  val size = compP[Dim](c ⇒ d ⇒ c.setSize(dimension(d)))

  val text = new Property[String,TextDisplay] {
    def := [B](a: String)(b: B)(implicit W: TextDisplay[B]) = W.setText(b, a)
  }

  val title = frameLikeP(_.setTitle)

  val tooltip =
    componentP[Option[String]](c ⇒ os ⇒ c.setToolTipText(os getOrElse null))

  val vAlign = new Property[VAlign,TextAlign] {
    def := [B](a: VAlign)(b: B)(implicit W: TextAlign[B]) = W.setVAlign(b, a)
  }

  val viewportBorder = scrollPaneP(_.setViewportBorder)

  val visible = compP(_.setVisible)

  val vScrollBarPolicy = scrollPaneP { p ⇒ h: VScrollBarPolicy ⇒ 
    p.setVerticalScrollBarPolicy(h.v) }

  val vTextPos = new Property[VAlign,TextAlign] {
    def := [B](a: VAlign)(b: B)(implicit W: TextAlign[B]) = W.setVTextPos(b, a)
  }

  val wheelScrollingEnabled = scrollPaneP(_.setWheelScrollingEnabled)

  val undecorated = frameLikeP(_.setUndecorated)
}

/** This trait is used to conveniently define properties with
  * a minimum of boilerplate.
  */
trait ~>>[F[_],R] {
  def apply[A](f: F[A], a: A): R
}

// vim: set ts=2 sw=2 et:
