package dire.swing

import dire._
import java.awt.{Font, Color, Image}
import javax.swing.Icon
import javax.swing.border.Border
import scalaz._, Scalaz._

object Swing extends Properties {

  final implicit class AbstractButtonOps[A](val a: A)
    (implicit F: AbstractButton[A]) {
    def clicks: SIn[Unit] = F clicks a
    def selected: Sink[Boolean] = F selected a
    def value: SIn[Boolean] = F value a
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

  final implicit class IconDisplayOps[A](val a: A)
    (implicit F: IconDisplay[A]) {

    def disabledIcon: Sink[Icon] = F disabledIcon a
    def icon: Sink[Icon] = F icon a
    def iconTextGap: Sink[Int] = F iconTextGap a
  }

  final implicit class TextDisplayOps[A](val a: A)
    (implicit F: TextDisplay[A]) {

    def hAlign: Sink[HAlign] = F hAlign a
    def hTextPos: Sink[HAlign] = F hTextPos a
    def text: Sink[String] = F text a
    def textA[B]: Sink[B] = F textA a
    def textS[B:Show]: Sink[B] = F textS a
    def vAlign: Sink[VAlign] = F vAlign a
    def vTextPos: Sink[VAlign] = F vTextPos a
  }

  final implicit class WindowOps[A](val a: A)(implicit F: Window[A]) {
    import Window.WindowEvent
    def iconImage: Sink[Image] = F iconImage a
    def iconImages: Sink[List[Image]] = F iconImages a
    def windowEvents: SIn[WindowEvent] = F windowEvents a
  }
}

// vim: set ts=2 sw=2 et:
