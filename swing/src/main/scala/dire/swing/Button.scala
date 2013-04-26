package dire.swing

import dire._
import javax.swing.JButton
import scalaz.effect.IO
import Swing.PropertySetOps

final class Button(val peer: JButton)

object Button {
  def apply(props: Button ⇒ IO[Unit]*): IO[Button] = for {
    res ← IO(new Button(new JButton()))
    _   ← res setList props.toList
  } yield res

  implicit val ButtonComponent = new AbstractButton[Button] {
    def peer(b: Button) = b.peer
    protected def isBlocked(a: Button) = false
    protected def block(a: Button, b: Boolean) {} 
  }

  implicit val ButtonElem: AsSingleElem[Button] = Elem hFill { _.peer }
}

// vim: set ts=2 sw=2 et:
