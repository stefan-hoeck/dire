package dire.swing

import dire._
import javax.swing.text.JTextComponent
import javax.swing.event.{DocumentListener, DocumentEvent}
import scalaz._, Scalaz._, effect.IO

trait TextComponent[A <: JTextComponent]
  extends Component[A]
  with BlockedSignal {
  import TextField._

  def text: Sink[String] = blockedSink(this)(peer.setText)

  def value: SIn[String] = SF cachedSrc this
}

object TextComponent {

  implicit def TextComponentSource[A<:JTextComponent]
    : Source[TextComponent[A],String] = 
    src[TextComponent[A],String](_.peer.getText) { t ⇒ o ⇒ 
      val l = listener(t, o)
      t.peer.getDocument.addDocumentListener(l)
      _ ⇒ t.peer.getDocument.removeDocumentListener(l)
    }

  private def listener[A<:JTextComponent](
    t: TextComponent[A], o: String ⇒ Unit) = new DocumentListener {
      def changedUpdate(e: DocumentEvent) { adjust() }
      def insertUpdate(e: DocumentEvent) { adjust() }
      def removeUpdate(e: DocumentEvent) { adjust() }

      private def adjust() { if (! t.blocked) o(t.peer.getText) }
    }
}

// vim: set ts=2 sw=2 et:
