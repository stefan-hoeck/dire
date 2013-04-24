package dire.swing

import dire._
import javax.swing.JButton
import scalaz._, Scalaz._, effect.IO

final class Button(val peer: JButton) extends BlockedSignal

object Button {
  def apply(props: Button ⇒ IO[Unit]*): IO[Button] = for {
    res ← IO(new Button(new JButton()))
    _   ← props.toList foldMap { _(res) }
  } yield res

  implicit val ButtonComponent = new AbstractButton[Button] {
    def peer(b: Button) = b.peer
    protected def isBlocked(a: Button) = a.blocked
    protected def setBlocked(a: Button, b: Boolean) { a.blocked = b } 
  }

  implicit val ButtonElem: AsSingleElem[Button] = Elem hFill { _.peer }
}

// vim: set ts=2 sw=2 et:
