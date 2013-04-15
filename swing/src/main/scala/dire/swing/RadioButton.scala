package dire.swing

import javax.swing.{JRadioButton}
import scalaz.effect.IO

case class RadioButton(peer: JRadioButton)
  extends AbstractButton[JRadioButton]

object RadioButton {
  def apply(text: String = "",
            selected: Boolean = false): IO[RadioButton] =
    IO(RadioButton(new JRadioButton(text, selected)))

  implicit val RadioButtonElem: AsSingleElem[RadioButton] =
    Elem hFill { _.peer }
}

// vim: set ts=2 sw=2 et:
