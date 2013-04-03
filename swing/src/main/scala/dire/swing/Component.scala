package dire.swing

import dire._, SF.EFOps
import java.awt.event.{MouseMotionListener, MouseEvent}
import javax.swing.JComponent

trait Component[A<:JComponent] extends Wrapped[A] {
  import Component._

  def mouseMoved: EIn[MotionEvent] = SF cachedSrc this

  def mousePosition: SIn[Position] = mouseMoved mapE { _.pos } hold ((0, 0))
}

object Component {

  implicit def ComponentSource[A<:JComponent]
    : ESource[Component[A],MotionEvent] = eventSrc { c ⇒ o ⇒ 
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
