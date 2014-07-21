package dire.swing

import dire._
import javax.swing.JTextArea
import scalaz._, Scalaz._, effect.IO

class TextArea(val peer: JTextArea) {
  private var blocked = false
}

object TextArea {
  def apply(props: (TextArea ⇒ IO[Unit])*): IO[TextArea] = for {
    res ← IO(new TextArea(new JTextArea()))
    _   ← props.toList foldMap { _(res) }
  } yield res

  implicit val TextAreaComponent = new TextComponent[TextArea] {
    def peer(b: TextArea): JTextArea = b.peer
    protected def isBlocked(a: TextArea) = a.blocked
    protected def block(a: TextArea, b: Boolean) { a.blocked = b } 
  }

  implicit val TextAreaElem: AsSingleElem[TextArea] = Elem vhFill { _.peer }
}

// vim: set ts=2 sw=2 et:
