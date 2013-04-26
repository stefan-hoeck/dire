package dire.swing

import dire._
import javax.swing.{JTextField}
import scalaz._, Scalaz._, effect.IO

class TextField(val peer: JTextField) {
  private var blocked = false
}

object TextField {
  def apply(props: TextField ⇒ IO[Unit]*): IO[TextField] = for {
    res ← IO(new TextField(new JTextField()))
    _   ← props.toList foldMap { _(res) }
  } yield res

  def text(t: String): IO[TextField] = apply(Swing.text := t)

  def trailing(t: String): IO[TextField] =
    apply(Swing.text := t, Swing.hAlign := HAlign.Trailing)

  implicit val TextFieldComponent = new TextFieldLike[TextField] {
    def peer(b: TextField): JTextField = b.peer
    protected def isBlocked(a: TextField) = a.blocked
    protected def block(a: TextField, b: Boolean) { a.blocked = b } 
  }

  implicit val TextFieldElem: AsSingleElem[TextField] = Elem hFill { _.peer }
}

// vim: set ts=2 sw=2 et:
