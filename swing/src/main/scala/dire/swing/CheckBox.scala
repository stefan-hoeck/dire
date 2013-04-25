package dire.swing

import dire._
import javax.swing.{JCheckBox}
import scalaz._, Scalaz._, effect.IO

class CheckBox(val peer: JCheckBox) {
  private var blocked = false
}

object CheckBox {
  def apply(props: CheckBox ⇒ IO[Unit]*): IO[CheckBox] = for {
    res ← IO(new CheckBox(new JCheckBox()))
    _   ← props.toList foldMap { _(res) }
  } yield res

  implicit val CheckBoxComponent = new AbstractButton[CheckBox] {
    def peer(b: CheckBox) = b.peer
    protected def isBlocked(a: CheckBox) = a.blocked
    protected def block(a: CheckBox, b: Boolean) { a.blocked = b } 
  }

  implicit val CheckBoxElem: AsSingleElem[CheckBox] = Elem hFill { _.peer }
}

// vim: set ts=2 sw=2 et:
