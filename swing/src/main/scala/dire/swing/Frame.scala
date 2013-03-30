package dire.swing

import dire._
import java.awt.BorderLayout
import java.awt.event.{WindowListener, WindowEvent}
import javax.swing.JFrame
import scalaz._, Scalaz._, effect.IO

case class FrameV(title: String)

class Frame private(ini: FrameV, private[swing] val peer: JFrame) {
  import Frame._

  private def display(fv: FrameV): IO[Unit] =
    IO(peer.setTitle(fv.title))

  def sf: EF[FrameV,FrameEvent] = SF sfCached this

  def events: EIn[FrameEvent] = SF const ini andThen sf

  def addElem(e: Elem): IO[Unit] = for {
    p ← e.panel
    _ ← IO(peer.add(p.peer, BorderLayout.NORTH))
  } yield ()

  def show: IO[Unit] = IO(peer.setVisible(true))
}

object Frame {
  def apply(): IO[Frame] = apply("")

  def apply(title: String): IO[Frame] = apply(FrameV(title))

  def apply(fv: FrameV): IO[Frame] = for {
    f ← IO(new Frame(fv, new JFrame))
    _ ← f display fv
  } yield f

  implicit val FrameSink: Sink[Frame,FrameV] = sink(_.display)

  implicit val FrameSource: ESource[Frame,FrameEvent] = eventSrc { f ⇒ o ⇒ 
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
