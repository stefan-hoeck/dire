package dire.swing

import dire._
import javax.swing.{JCheckBox}
import scalaz.effect.IO

case class CheckBox(peer: JCheckBox) extends AbstractButton[JCheckBox]

object CheckBox {
  def apply(text: String = "",
            selected: Boolean = false): IO[CheckBox] =
    IO(CheckBox(new JCheckBox(text, selected)))

  implicit val CheckBoxElem: AsSingleElem[CheckBox] = Elem hFill { _.peer }
}

// vim: set ts=2 sw=2 et:
