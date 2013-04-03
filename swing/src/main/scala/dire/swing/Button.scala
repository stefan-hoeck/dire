package dire.swing

import dire._
import javax.swing.JButton
import scalaz._, Scalaz._, effect.IO

case class Button(peer: JButton) extends Wrapped[JButton] {
  import Button._

  def clicks: EIn[Unit] = SF cachedSrc this

  def text: EF[Event[String],Nothing] = sink(this){ peer.setText }
}

object Button {
  def apply(): IO[Button] = apply("")

  def apply(text: String): IO[Button] = IO(Button(new JButton(text)))

  implicit val ButtonSource: ESource[Button,Unit] = eventSrc { b ⇒ o ⇒ 
    val a = ali(o)
    b.peer.addActionListener(a)
    _ ⇒ b.peer.removeActionListener(a)
  }

  implicit val ButtonElem: AsElem[Button] = Elem hFill { _.peer }
}

// vim: set ts=2 sw=2 et:
