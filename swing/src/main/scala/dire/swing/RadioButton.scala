package dire.swing

import dire._
import javax.swing.{JRadioButton}
import scalaz._, Scalaz._, effect.IO

class RadioButton(val peer: JRadioButton) {
  private var blocked = false
}

object RadioButton {
  def apply(props: RadioButton ⇒ IO[Unit]*): IO[RadioButton] = for {
    res ← IO(new RadioButton(new JRadioButton()))
    _   ← props.toList foldMap { _(res) }
  } yield res

  implicit val RadioButtonComponent = new AbstractButton[RadioButton] {
    def peer(b: RadioButton) = b.peer
    protected def isBlocked(a: RadioButton) = a.blocked
    protected def block(a: RadioButton, b: Boolean) { a.blocked = b } 
  }

  implicit val RadioButtonElem: AsSingleElem[RadioButton] =
    Elem hFill { _.peer }
}

// vim: set ts=2 sw=2 et:
