package dire.swing

import dire._
import java.awt.{Font, Color, Image}
import javax.swing.Icon
import javax.swing.text.Caret
import scala.reflect.runtime.universe.TypeTag
import scalaz._, Scalaz._, effect.IO
import Window.WindowEvent

object Swing
  extends Properties 
  with AsElemInstances
  with AsElemSyntax {

  final implicit class SwingOps[A](val a: A) extends AnyVal {
    import Component._
    def actionEvents(implicit F: FiresActionEvent[A]): SIn[Unit] = F actionEvents a
    def background(implicit F: Comp[A]): Sink[Color] = F background a
    def border(implicit F: Component[A]): Sink[Border] = F border a
    def bounds(implicit F: Comp[A]): Sink[Rect] = F bounds a
    def cachedIn[B:TypeTag](implicit F: IOWidget[A,B]): SIn[B] = F cachedIn a
    def cachedSf[B:TypeTag](implicit F: IOWidget[A,B]): SF[B,B] = F cachedSf a
    def caret(implicit F: TextComponent[A]): Sink[Caret] = F caret a
    def caretColor(implicit F: TextComponent[A]): Sink[Color] = F caretColor a
    def caretPosition(implicit F: TextComponent[A]): Sink[Int] = F caretPosition a
    def clicks(implicit F: AbstractButton[A]): SIn[Unit] = F clicks a
    def columns(implicit F: TextFieldLike[A]): Sink[Int] = F columns a
    def cursor(implicit F: Comp[A]): Sink[Cursor] = F cursor a
    def disabledIcon(implicit F: IconDisplay[A]): Sink[Icon] = F disabledIcon a
    def disabledTextColor(implicit F: TextComponent[A]): Sink[Color] = F disabledTextColor a
    def doubleBuffered(implicit F: Component[A]): Sink[Boolean] = F doubleBuffered a
    def echoChar(implicit F: PasswordDisplay[A]): Sink[Char] = F echoChar a
    def editable(implicit F: Editable[A]): Sink[Boolean] = F editable a
    def enabled(implicit F: Comp[A]): Sink[Boolean] = F enabled a
    def focus(implicit F: Comp[A]): SIn[FocusEvent] = F focus a
    def focusGained(implicit F: Comp[A]): SIn[Unit] = F focusGained a
    def focusLost(implicit F: Comp[A]): SIn[Unit] = F focusLost a
    def focusable(implicit F: Comp[A]): Sink[Boolean] = F focusable a
    def font(implicit F: Comp[A]): Sink[Font] = F font a
    def foreground(implicit F: Comp[A]): Sink[Color] = F foreground a
    def hAlign(implicit F: TextAlign[A]): Sink[HAlign] = F hAlign a
    def hScrollBarPolicy(implicit F: ScrollPaneLike[A]): Sink[HScrollBarPolicy] = F hScrollBarPolicy a
    def hTextPos(implicit F: TextAlign[A]): Sink[HAlign] = F hTextPos a
    def icon(implicit F: IconDisplay[A]): Sink[Icon] = F icon a
    def iconImage(implicit F: Window[A]): Sink[Image] = F iconImage a
    def iconImages(implicit F: Window[A]): Sink[List[Image]] = F iconImages a
    def iconTextGap(implicit F: IconDisplay[A]): Sink[Int] = F iconTextGap a
    def in[B](implicit F: IOWidget[A,B]): SIn[B] = F in a
    def itemInO[B](implicit F: ComboBoxLike[A,B]): SIn[Option[B]] = F itemInO a
    def itemO[B](implicit F: ComboBoxLike[A,B]): Sink[Option[B]] = F itemO a
    def items[B,F[_]:Foldable](implicit F: ComboBoxLike[A,B]): Sink[F[B]] = F items a
    def keys(implicit F: Comp[A]): SIn[KeyEvent] = F keys a
    def leftClicks(implicit F: Comp[A]): SIn[Unit] = F leftClicks a
    def maxSize(implicit F: Comp[A]): Sink[Dim] = F maxSize a
    def maximizedBounds(implicit F: FrameLike[A]): Sink[Rect] = F maximizedBounds a
    def minSize(implicit F: Comp[A]): Sink[Dim] = F minSize a
    def mouse(implicit F: Comp[A]): SIn[MouseEvent] = F mouse a
    def mouseMoved(implicit F: Comp[A]): SIn[MotionEvent] = F mouseMoved a
    def mousePosition(implicit F: Comp[A]): SIn[Position] = F mousePosition a
    def name(implicit F: Comp[A]): Sink[String] = F name a
    def opaque(implicit F: Component[A]): Sink[Boolean] = F opaque a
    def out[B](implicit F: IOWidget[A,B]): Sink[B] = F out a
    def password(implicit F: PasswordDisplay[A]): Sink[String] = F password a
    def preferredSize(implicit F: Comp[A]): Sink[Dim] = F preferredSize a
    def properties(props: (A ⇒ IO[Unit])*): IO[Unit] = setList(props.toList)
    def resizable(implicit F: FrameLike[A]): Sink[Boolean] = F resizable a
    def rightClicks(implicit F: Comp[A]): SIn[Unit] = F rightClicks a
    def selected(implicit F: AbstractButton[A]): Sink[Boolean] = F selected a
    def selectedTextColor(implicit F: TextComponent[A]): Sink[Color] = F selectedTextColor a
    def selectionColor(implicit F: TextComponent[A]): Sink[Color] = F selectionColor a
    def selectionEnd(implicit F: TextComponent[A]): Sink[Int] = F selectionEnd a
    def selectionStart(implicit F: TextComponent[A]): Sink[Int] = F selectionStart a
    def setList(props: List[A ⇒ IO[Unit]]): IO[Unit] = props foldMap { _ apply a }
    def sf[B](implicit F: IOWidget[A,B]): SF[B,B] = F sf a
    def size(implicit F: Comp[A]): Sink[Dim] = F size a
    def text(implicit F: TextDisplay[A]): Sink[String] = F text a
    def textA[B](implicit F: TextDisplay[A]): Sink[B] = F textA a
    def textEvents(implicit F: TextFieldLike[A]): SIn[String] = F textEvents a
    def textIn(implicit F: TextComponent[A]): SIn[String] = F in a
    def textInComboBox(implicit F: ComboBoxLike[A,String]): SIn[String] = F textIn a
    def textS[B:Show](implicit F: TextDisplay[A]): Sink[B] = F textS a
    def title(implicit F: FrameLike[A]): Sink[String] = F title a
    def tooltip(implicit F: Component[A]): Sink[Option[String]] = F tooltip a
    def undecorated(implicit F: FrameLike[A]): Sink[Boolean] = F undecorated a
    def vAlign(implicit F: TextAlign[A]): Sink[VAlign] = F vAlign a
    def viewportBorder(implicit F: ScrollPaneLike[A]): Sink[Border] = F viewportBorder a
    def vScrollBarPolicy(implicit F: ScrollPaneLike[A]): Sink[VScrollBarPolicy] = F vScrollBarPolicy a
    def vTextPos(implicit F: TextAlign[A]): Sink[VAlign] = F vTextPos a
    def visible(implicit F: Comp[A]): Sink[Boolean] = F visible a
    def wheelScrollingEnabled(implicit F: ScrollPaneLike[A]): Sink[Boolean] = F wheelScrollingEnabled a
    def windowEvents(implicit F: Window[A]): SIn[WindowEvent] = F windowEvents a
  }

  final implicit class PairOps[A,B,F[_]](val ps: F[(A,B)]) extends AnyVal {
    def group(implicit A: AbstractButton[A], F: Foldable[F], B: Equal[B])
      : IO[SF[B,B]] = A group ps
  }
}

// vim: set ts=2 sw=2 et nowrap:
