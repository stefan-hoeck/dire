package dire.swing

import dire._
import java.awt.{Window ⇒ AWindow, Image}
import java.awt.event.{WindowListener, WindowEvent ⇒ JWindowEvent}
import scalaz._, Scalaz._, effect.IO

/** Type class representing a `java.awt.Window` */
trait Window[-A] extends Container[A] {
  import Window._

  def peer(a: A): AWindow

  override def adjustSize(a: A, f: Dim ⇒ Dim): IO[Unit] = IO {
    peer(a).pack()
    val old = peer(a).getSize
    peer(a).setSize(dimension(f(old.width, old.height)))
  }

  final def iconImage(a: A): Sink[Image] = sink(peer(a).setIconImage)

  final def iconImages(a: A): Sink[List[Image]] = {
    import scala.collection.JavaConversions._

    sink(is ⇒ peer(a).setIconImages(is))
  }

  final def windowEvents(a: A): SIn[WindowEvent] = SF cachedSrc peer(a)
}

object Window {
  def apply[A:Window]: Window[A] = implicitly

  implicit val WindowIso = new (Window ~>> AWindow) {
    def apply[A](f: Window[A], a: A) = f peer a
  }

  sealed trait WindowEvent

  case object Activated extends WindowEvent
  case object Closed extends WindowEvent
  case object Closing extends WindowEvent
  case object Deactivated extends WindowEvent
  case object Deiconified extends WindowEvent
  case object Iconified extends WindowEvent
  case object Opened extends WindowEvent

  implicit val WindowSource: Source[AWindow,WindowEvent] = eventSrc { w ⇒ o ⇒ 
    val l = listener(o)
    w.addWindowListener(l)
    _ ⇒ w.removeWindowListener(l)
  }

  private def listener(o: WindowEvent ⇒ Unit) = new WindowListener {
    def windowActivated(e: JWindowEvent) {o(Activated)}
    def windowClosed(e: JWindowEvent) {o(Closed)}
    def windowClosing(e: JWindowEvent) {o(Closing)}
    def windowDeactivated(e: JWindowEvent) {o(Deactivated)}
    def windowDeiconified(e: JWindowEvent) {o(Deiconified)}
    def windowIconified(e: JWindowEvent) {o(Iconified)}
    def windowOpened(e: JWindowEvent) {o(Opened)}
  }
}

// vim: set ts=2 sw=2 et:
