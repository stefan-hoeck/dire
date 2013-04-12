package dire.swing

import dire._
import java.awt.{BorderLayout, Color, Dimension}
import java.awt.event.{WindowListener, WindowEvent}
import javax.swing.JFrame
import scalaz._, Scalaz._, effect.IO

case class Frame(peer: JFrame) extends Wrapped[JFrame] {
  import Frame._

  def events: SIn[FrameEvent] = SF cachedSrc this

  def title: Sink[String] = sink(peer.setTitle, this)

  def addElem(e: Elem, l: FrameLayout = Center): IO[Unit] = for {
    p  ← e.panel
    d  ← IO { peer.add(p.peer, l.v); peer.pack(); peer.getSize }
    nd = e prefSize (d.width, d.height)
    _  ← IO(peer.setSize(new Dimension(nd._1, nd._2)))
  } yield ()

  def show: IO[Unit] = IO { peer.setVisible(true) }
}

object Frame {
  def apply(): IO[Frame] = apply("")

  def apply(title: String): IO[Frame] = IO(Frame(new JFrame(title)))

  implicit val FrameSource: Source[Frame,FrameEvent] = eventSrc { f ⇒ o ⇒ 
    val l = listener(o)
    f.peer.addWindowListener(l)
    _ ⇒ f.peer.removeWindowListener(l)
  }

  sealed trait FrameEvent

  case object Activated extends FrameEvent
  case object Closed extends FrameEvent
  case object Closing extends FrameEvent
  case object Deactivated extends FrameEvent
  case object Deiconified extends FrameEvent
  case object Iconified extends FrameEvent
  case object Opened extends FrameEvent

  sealed abstract class FrameLayout(val v: String)

  case object Center extends FrameLayout(BorderLayout.CENTER)
  case object North extends FrameLayout(BorderLayout.NORTH)
  case object East extends FrameLayout(BorderLayout.EAST)
  case object South extends FrameLayout(BorderLayout.SOUTH)
  case object West extends FrameLayout(BorderLayout.WEST)

  private def listener(o: FrameEvent ⇒ Unit) = new WindowListener {
    def windowActivated(e: WindowEvent) {o(Activated)}
    def windowClosed(e: WindowEvent) {o(Closed)}
    def windowClosing(e: WindowEvent) {o(Closing)}
    def windowDeactivated(e: WindowEvent) {o(Deactivated)}
    def windowDeiconified(e: WindowEvent) {o(Deiconified)}
    def windowIconified(e: WindowEvent) {o(Iconified)}
    def windowOpened(e: WindowEvent) {o(Opened)}
  }
}

// vim: set ts=2 sw=2 et:
