package dire.swing

import dire._
import javax.swing.JTextField
import javax.swing.event.{DocumentListener, DocumentEvent}
import scalaz._, Scalaz._, effect.IO

case class TextField(peer: JTextField)
  extends Component[JTextField] 
  with BlockedSignal {
  import TextField._

  def text: EF[Event[String],Nothing] = blockedSink(this){ peer.setText }

  def value: SIn[String] = SF cachedSrc this
}

object TextField {
  def apply(): IO[TextField] = apply("")

  def apply(text: String): IO[TextField] =
    IO(TextField(new JTextField(text)))

  implicit val TextFieldSource: Source[TextField,String] = 
    src[TextField,String](_.peer.getText) { t ⇒ o ⇒ 
      val l = listener(t, o)
      t.peer.getDocument.addDocumentListener(l)
      _ ⇒ t.peer.getDocument.removeDocumentListener(l)
    }

  implicit val TextFieldElem: AsElem[TextField] = Elem hFill { _.peer }

  private def listener(t: TextField, o: String ⇒ Unit) =
    new DocumentListener {
      def changedUpdate(e: DocumentEvent) { adjust() }
      def insertUpdate(e: DocumentEvent) { adjust() }
      def removeUpdate(e: DocumentEvent) { adjust() }

      private def adjust() { if (! t.blocked) o(t.peer.getText) }
    }
}

// vim: set ts=2 sw=2 et:
