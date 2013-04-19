package dire.swing

import dire._
import java.awt.{Font, Color}
import java.awt.event.{MouseMotionListener, MouseListener, KeyListener,
                       MouseEvent ⇒ JMouseEvent, KeyEvent ⇒ JKeyEvent,
                       FocusEvent ⇒ JFocusEvent, FocusListener} 
import javax.swing.JComponent
import javax.swing.border.Border

trait Component[A<:JComponent] extends Wrapped[A] {
  import Component._

  def name: Sink[String] = sink(peer.setName, this)

  def opaque: Sink[Boolean] = sink(peer.setOpaque, this)

  def background: Sink[Color] = sink(peer.setBackground, this)

  def border: Sink[Border] = sink(peer.setBorder, this)

  def enabled: Sink[Boolean] = sink(peer.setEnabled, this)

  def focusable: Sink[Boolean] = sink(peer.setFocusable, this)

  def focus: SIn[FocusEvent] = SF.cachedSrc[Component[A],FocusEvent](this)

  def focusGained: SIn[Unit] = focus collect { case FocusGained ⇒ () }

  def focusLost: SIn[Unit] = focus collect { case FocusLost ⇒ () }

  def font: Sink[Font] = sink(peer.setFont, this)

  def foreground: Sink[Color] = sink(peer.setForeground, this)

  def keys: SIn[KeyEvent] = SF.cachedSrc[Component[A],KeyEvent](this)

  def leftClicks: SIn[Unit] = mouse collect { case Clicked(Button1) ⇒ () }

  def mouse: SIn[MouseEvent] = SF.cachedSrc[Component[A],MouseEvent](this)

  def mouseMoved: SIn[MotionEvent] =
    SF.cachedSrc[Component[A],MotionEvent](this)

  def mousePosition: SIn[Position] = mouseMoved map { _.pos }

  def rightClicks: SIn[Unit] = mouse collect { case Clicked(Button3) ⇒ () }

  def tooltip: Sink[String] = sink(peer.setToolTipText, this)
}

object Component {

  implicit def MotionSource[A<:JComponent]: Source[Component[A],MotionEvent] =
    eventSrc { c ⇒ o ⇒ 
      val l = motionListener(o)
      c.peer.addMouseMotionListener(l)
      _ ⇒ c.peer.removeMouseMotionListener(l)
    }

  implicit def MouseSource[A<:JComponent]: Source[Component[A],MouseEvent] =
    eventSrc { c ⇒ o ⇒ 
      val l = mouseListener(o)
      c.peer.addMouseListener(l)
      _ ⇒ c.peer.removeMouseListener(l)
    }

  implicit def KeySource[A<:JComponent]: Source[Component[A],KeyEvent] =
    eventSrc { c ⇒ o ⇒ 
      val l = keyListener(o)
      c.peer.addKeyListener(l)
      _ ⇒ c.peer.removeKeyListener(l)
    }

  implicit def FocusSource[A<:JComponent]: Source[Component[A],FocusEvent] =
    eventSrc { c ⇒ o ⇒ 
      val l = focusListener(o)
      c.peer.addFocusListener(l)
      _ ⇒ c.peer.removeFocusListener(l)
    }

  sealed trait MotionEvent { def pos: Position }

  case class Moved(pos: Position) extends MotionEvent
  case class Dragged(pos: Position) extends MotionEvent

  private def motionListener(o: MotionEvent ⇒ Unit) = new MouseMotionListener {
    def mouseDragged(x: JMouseEvent) { o(Dragged((x.getX, x.getY))) }
    def mouseMoved(x: JMouseEvent) { o(Moved((x.getX, x.getY))) }
  }

  sealed trait MouseEvent

  case class Pressed(button: MouseButton) extends MouseEvent
  case class Released(button: MouseButton) extends MouseEvent
  case class Clicked(button: MouseButton) extends MouseEvent
  case object Entered extends MouseEvent
  case object Exited extends MouseEvent

  sealed abstract class MouseButton(val v: Int)

  case object Button1 extends MouseButton(JMouseEvent.BUTTON1)
  case object Button2 extends MouseButton(JMouseEvent.BUTTON2)
  case object Button3 extends MouseButton(JMouseEvent.BUTTON3)
  case object NoButton extends MouseButton(JMouseEvent.NOBUTTON)

  private def button(e: JMouseEvent): MouseButton = e.getButton match {
    case JMouseEvent.BUTTON1 ⇒ Button1
    case JMouseEvent.BUTTON2 ⇒ Button2
    case JMouseEvent.BUTTON3 ⇒ Button3
    case _                   ⇒ NoButton
  }

  private def mouseListener(o: MouseEvent ⇒ Unit) = new MouseListener {
    def mouseClicked(e: JMouseEvent) { o(Clicked(button(e))) }
    def mousePressed(e: JMouseEvent) { o(Pressed(button(e))) }
    def mouseReleased(e: JMouseEvent) { o(Released(button(e))) }
    def mouseEntered(e: JMouseEvent) { o(Entered) }
    def mouseExited(e: JMouseEvent) { o(Exited) }
  }

  sealed trait KeyEvent {
    def mods: Int
    def loc: KeyLocation
  }

  case class KeyTyped(char: Char, mods: Int, loc: KeyLocation)
    extends KeyEvent {
    def this(e: JKeyEvent) = this(e.getKeyChar,
      e.getModifiersEx, KeyLocation get e.getKeyLocation)
  }

  case class KeyPressed(key: Key, mods: Int, loc: KeyLocation)
    extends KeyEvent {
    def this(e: JKeyEvent) = this(Key get e.getKeyCode,
      e.getModifiersEx, KeyLocation get e.getKeyLocation)
  }

  case class KeyReleased(key: Key, mods: Int, loc: KeyLocation)
    extends KeyEvent {
    def this(e: JKeyEvent) = this(Key get e.getKeyCode,
      e.getModifiersEx, KeyLocation get e.getKeyLocation)
  }

  private def keyListener(o: KeyEvent ⇒ Unit) = new KeyListener {
    def keyPressed(e: JKeyEvent) { o(new KeyPressed(e)) }
    def keyReleased(e: JKeyEvent) { o(new KeyReleased(e)) }
    def keyTyped(e: JKeyEvent) { o(new KeyTyped(e)) }
  }

  sealed trait FocusEvent

  case object FocusGained extends FocusEvent
  case object FocusLost extends FocusEvent

  private def focusListener(o: FocusEvent ⇒ Unit) = new FocusListener {
    def focusGained(e: JFocusEvent) { o(FocusGained) }
    def focusLost(e: JFocusEvent) { o(FocusLost) }
  }
}

// vim: set ts=2 sw=2 et:
