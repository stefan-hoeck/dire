package dire.swing

import dire._
import javax.swing.JButton
import scalaz._, Scalaz._, effect.IO

case class Button(peer: JButton) extends Component[JButton] {
  import Button._

  def clicks: SIn[Unit] = SF cachedSrc this

  def text: Sink[String] = sink(peer.setText, this)
}

object Button {
  def apply(): IO[Button] = apply("")

  def apply(text: String): IO[Button] = IO(Button(new JButton(text)))

  implicit val ButtonSource: Source[Button,Unit] = eventSrc { b ⇒ o ⇒ 
    val a = ali(o)
    b.peer.addActionListener(a)
    _ ⇒ b.peer.removeActionListener(a)
  }

  implicit val ButtonElem: AsSingleElem[Button] = Elem hFill { _.peer }
}

// vim: set ts=2 sw=2 et:
