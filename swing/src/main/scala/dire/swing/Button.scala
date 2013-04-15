package dire.swing

import dire._
import javax.swing.JButton
import scalaz._, Scalaz._, effect.IO

case class Button(peer: JButton) extends AbstractButton[JButton]

object Button {
  def apply(text: String = ""): IO[Button] =
    IO(Button(new JButton(text)))

  implicit val ButtonElem: AsSingleElem[Button] = Elem hFill { _.peer }
}

// vim: set ts=2 sw=2 et:
