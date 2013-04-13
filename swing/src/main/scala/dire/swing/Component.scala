package dire.swing

import dire._
import java.awt.{Font, Color}
import java.awt.event.{MouseMotionListener, MouseListener,
                       MouseEvent ⇒ JMouseEvent} 
import javax.swing.JComponent

trait Component[A<:JComponent] extends Wrapped[A] {
  import Component._

  def background: Sink[Color] = sink(peer.setBackground, this)

  def enabled: Sink[Boolean] = sink(peer.setEnabled, this)

  def font: Sink[Font] = sink(peer.setFont, this)

  def foreground: Sink[Color] = sink(peer.setForeground, this)

  def leftClicks: SIn[Unit] = mouse collect { case Clicked(Button1) ⇒ () }

  def mouse: SIn[MouseEvent] =
    SF.cachedSrc[Component[A],MouseEvent](this)

  def mouseMoved: SIn[MotionEvent] =
    SF.cachedSrc[Component[A],MotionEvent](this)

  def mousePosition: SIn[Position] = mouseMoved map { _.pos }

  def rightClicks: SIn[Unit] = mouse collect { case Clicked(Button2) ⇒ () }

  def tooltip: Sink[String] = sink(peer.setToolTipText, this)
}

object Component {

  implicit def MotionSource[A<:JComponent]
    : Source[Component[A],MotionEvent] = eventSrc { c ⇒ o ⇒ 
      val l = motionListener(o)
      c.peer.addMouseMotionListener(l)
      _ ⇒ c.peer.removeMouseMotionListener(l)
    }

  implicit def MouseSource[A<:JComponent]
    : Source[Component[A],MouseEvent] = eventSrc { c ⇒ o ⇒ 
      val l = mouseListener(o)
      c.peer.addMouseListener(l)
      _ ⇒ c.peer.removeMouseListener(l)
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
}

// vim: set ts=2 sw=2 et:
