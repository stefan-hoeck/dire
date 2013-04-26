package dire.swing

import dire._
import java.awt.{Font, Color, Image}
import javax.swing.Icon
import javax.swing.border.Border
import javax.swing.text.Caret
import scalaz._, Scalaz._, effect.IO

object Swing
  extends Properties 
  with AsElemInstances
  with AsElemSyntax {

  final implicit class AbstractButtonOps[A](val a: A)
    (implicit F: AbstractButton[A]) {
    def clicks: SIn[Unit] = F clicks a
    def selected: Sink[Boolean] = F selected a
  }

  final implicit class CompOps[A](val a: A)(implicit F: Comp[A]) {
    import Component._
    def background: Sink[Color] = F background a
    def bounds: Sink[Rect] = F bounds a
    def cursor: Sink[Cursor] = F cursor a
    def enabled: Sink[Boolean] = F enabled a
    def focus: SIn[FocusEvent] = F focus a
    def focusGained: SIn[Unit] = F focusGained a
    def focusLost: SIn[Unit] = F focusLost a
    def focusable: Sink[Boolean] = F focusable a
    def font: Sink[Font] = F font a
    def foreground: Sink[Color] = F foreground a
    def keys: SIn[KeyEvent] = F keys a
    def leftClicks: SIn[Unit] = F leftClicks a
    def maxSize: Sink[Dim] = F maxSize a
    def minSize: Sink[Dim] = F minSize a
    def mouse: SIn[MouseEvent] = F mouse a
    def mouseMoved: SIn[MotionEvent] = F mouseMoved a
    def mousePosition: SIn[Position] = F mousePosition a
    def name: Sink[String] = F name a
    def preferredSize: Sink[Dim] = F preferredSize a
    def rightClicks: SIn[Unit] = F rightClicks a
    def size: Sink[Dim] = F size a
    def visible: Sink[Boolean] = F visible a
  }

  final implicit class ComponentOps[A](val a: A)(implicit F: Component[A]) {
    def border: Sink[Border] = F border a
    def doubleBuffered: Sink[Boolean] = F doubleBuffered a
    def opaque: Sink[Boolean] = F opaque a
    def tooltip: Sink[Option[String]] = F tooltip a
  }

  final implicit class FireActionEventOps[A](val a: A)
    (implicit F: FiresActionEvent[A]) {
    def actionEvents: SIn[Unit] = F actionEvents a
  }

  final implicit class FrameLikeOps[A](val a: A)
    (implicit F: FrameLike[A]) {
    def maximizedBounds: Sink[Rect] = F maximizedBounds a
    def resizable: Sink[Boolean] = F resizable a
    def title: Sink[String] = F title a
    def undecorated: Sink[Boolean] = F undecorated a
  }

  final implicit class IconDisplayOps[A](val a: A)
    (implicit F: IconDisplay[A]) {

    def disabledIcon: Sink[Icon] = F disabledIcon a
    def icon: Sink[Icon] = F icon a
    def iconTextGap: Sink[Int] = F iconTextGap a
  }

  final implicit class IOWidgetOps[A,B](val a: A)
    (implicit F: IOWidget[A,B]) {

    def in: SIn[B] = F in a
    def out: Sink[B] = F out a
    def sf: SF[B,B] = F sf a
  }

  final implicit class PasswordDisplayOps[A](val a: A)
    (implicit F: PasswordDisplay[A]) {
    def echoChar: Sink[Char] = F echoChar a
    def password: Sink[String] = F password a
  }

  final implicit class PropertySetOps[A](val a: A) {
    def properties(props: A ⇒ IO[Unit]*): IO[Unit] = 
      setList(props.toList)

    def setList(props: List[A ⇒ IO[Unit]]): IO[Unit] = 
      props foldMap { _ apply a }
  }

  final implicit class TextAlignOps[A](val a: A)
    (implicit F: TextAlign[A]) {

    def hAlign: Sink[HAlign] = F hAlign a
    def hTextPos: Sink[HAlign] = F hTextPos a
    def vAlign: Sink[VAlign] = F vAlign a
    def vTextPos: Sink[VAlign] = F vTextPos a
  }

  final implicit class TextComponentOps[A](val a: A)
    (implicit F: TextComponent[A]) {
    def caret: Sink[Caret] = F caret a
    def caretColor: Sink[Color] = F caretColor a
    def caretPosition: Sink[Int] = F caretPosition a
    def disabledTextColor: Sink[Color] = F disabledTextColor a
    def editable: Sink[Boolean] = F editable a
    def selectedTextColor: Sink[Color] = F selectedTextColor a
    def selectionColor: Sink[Color] = F selectionColor a
    def selectionEnd: Sink[Int] = F selectionEnd a
    def selectionStart: Sink[Int] = F selectionStart a
    def textIn: SIn[String] = F in a
  }

  final implicit class TextDisplayOps[A](val a: A)
    (implicit F: TextDisplay[A]) {

    def text: Sink[String] = F text a
    def textA[B]: Sink[B] = F textA a
    def textS[B:Show]: Sink[B] = F textS a
  }

  final implicit class TextFieldLikeOps[A](val a: A)
    (implicit F: TextFieldLike[A]) {
    def columns: Sink[Int] = F columns a
    def textEvents: SIn[String] = F textEvents a
  }

  final implicit class WindowOps[A](val a: A)(implicit F: Window[A]) {
    import Window.WindowEvent
    def iconImage: Sink[Image] = F iconImage a
    def iconImages: Sink[List[Image]] = F iconImages a
    def windowEvents: SIn[WindowEvent] = F windowEvents a
  }
}

// vim: set ts=2 sw=2 et:
