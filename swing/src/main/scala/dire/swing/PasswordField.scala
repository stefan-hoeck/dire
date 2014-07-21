package dire.swing

import javax.swing.JPasswordField
import scalaz._, Scalaz._, effect.IO

final class PasswordField(val peer: JPasswordField) {
  private var blocked = false
}

object PasswordField {
  def apply(props: (PasswordField ⇒ IO[Unit])*): IO[PasswordField] = for {
    res ← IO(new PasswordField(new JPasswordField()))
    _   ← props.toList foldMap { _(res) }
  } yield res

  implicit val PasswordFieldComponent =
    new TextFieldLike[PasswordField] 
    with PasswordDisplay[PasswordField] {
    def peer(b: PasswordField): JPasswordField = b.peer
    protected def isBlocked(a: PasswordField) = a.blocked
    protected def block(a: PasswordField, b: Boolean) { a.blocked = b } 
    override def setEchoChar(a: PasswordField, c: Char): IO[Unit] =
      IO(peer(a).setEchoChar(c))

    override def setPassword(a: PasswordField, s: String): IO[Unit] =
      IO(peer(a).setText(s))
  }

  implicit val PasswordFieldElem: AsSingleElem[PasswordField] =
    Elem hFill { _.peer }
}

// vim: set ts=2 sw=2 et:
