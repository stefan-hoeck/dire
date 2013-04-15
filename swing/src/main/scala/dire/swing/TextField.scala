package dire.swing

import dire._
import javax.swing.JTextField
import scalaz._, Scalaz._, effect.IO

case class TextField(peer: JTextField) extends TextComponent[JTextField] {
  import TextField._

  def actions: SIn[Unit] = SF cachedSrc this

  def textEvents: SIn[String] = value on actions
}

object TextField {
  def apply(text: String = ""): IO[TextField] =
    IO(TextField(new JTextField(text)))

  implicit val TextFieldElem: AsSingleElem[TextField] = Elem hFill { _.peer }

  implicit val TextFieldSource: Source[TextField,Unit] = eventSrc { t ⇒ o ⇒ 
    val a = ali(o)
    t.peer.addActionListener(a)
    _ ⇒ t.peer.removeActionListener(a)
  }
}

// vim: set ts=2 sw=2 et:
