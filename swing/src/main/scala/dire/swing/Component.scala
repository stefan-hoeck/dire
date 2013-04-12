package dire.swing

import dire._
import java.awt.event.{MouseMotionListener, MouseEvent}
import javax.swing.JComponent

trait Component[A<:JComponent] extends Wrapped[A] {
  import Component._

  def enabled: Sink[Boolean] = sink(peer.setEnabled, this)

  def mouseMoved: SIn[MotionEvent] = SF cachedSrc this

  def mousePosition: SIn[Position] = mouseMoved map { _.pos } hold (0, 0)

  def tooltip: Sink[String] = sink(peer.setToolTipText, this)
}

object Component {

  implicit def ComponentSource[A<:JComponent]
    : Source[Component[A],MotionEvent] = eventSrc { c ⇒ o ⇒ 
      val l = listener(o)
      c.peer.addMouseMotionListener(l)
      _ ⇒ c.peer.removeMouseMotionListener(l)
    }

  sealed trait MotionEvent {
    def pos: Position
  }

  case class Moved(pos: Position) extends MotionEvent
  case class Dragged(pos: Position) extends MotionEvent

  private def listener(o: MotionEvent ⇒ Unit) = new MouseMotionListener {
    def mouseDragged(x: MouseEvent) { o(Dragged((x.getX, x.getY))) }
    def mouseMoved(x: MouseEvent) { o(Moved((x.getX, x.getY))) }
  }
}

// vim: set ts=2 sw=2 et:
