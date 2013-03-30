package dire.swing

import dire._
import javax.swing.JButton
import scalaz._, Scalaz._, effect.IO

case class ButtonV(text: String)

class Button private(ini: ButtonV, private val peer: JButton) {
  import Button._

  private def display(bv: ButtonV): IO[Unit] =
    IO(peer.setText(bv.text))

  def sf: EF[ButtonV,Unit] = SF sfCached this

  def clicks: EIn[Unit] = SF const ini andThen sf
}

object Button {
  def apply(): IO[Button] = apply("")

  def apply(text: String): IO[Button] = apply(ButtonV(text))

  def apply(bv: ButtonV): IO[Button] = for {
    b ← IO(new Button(bv, new JButton))
    _ ← b display bv
  } yield b

  implicit val ButtonSink: Sink[Button,ButtonV] = sink(_.display)

  implicit val ButtonSource: ESource[Button,Unit] = eventSrc { b ⇒ o ⇒ 
    val a = ali(o)
    b.peer.addActionListener(a)
    _ ⇒ b.peer.removeActionListener(a)
  }

  implicit val ButtonElem: AsElem[Button] = Elem hFill { _.peer }
}

// vim: set ts=2 sw=2 et:
