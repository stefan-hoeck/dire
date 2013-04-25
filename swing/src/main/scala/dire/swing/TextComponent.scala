package dire.swing

import dire._
import javax.swing.text.JTextComponent
import javax.swing.event.{DocumentListener, DocumentEvent}
import scalaz._, Scalaz._, effect.IO

trait TextComponent[A] extends TextDisplay[A] with Blockable[A] {
  def peer(a: A): JTextComponent
//  def text: Sink[String] = blockedSink(this)(peer.setText)
//
//  def value: SIn[String] = SF cachedSrc this

  private implicit lazy val src: Source[A,String] = 
    dire.swing.src[A,String](a ⇒ peer(a).getText) { a ⇒ o ⇒ 
      val l = listener(a, o)
      peer(a).getDocument.addDocumentListener(l)
      _ ⇒ peer(a).getDocument.removeDocumentListener(l)
    }

  private def listener(a: A, o: String ⇒ Unit) =
    new DocumentListener {
      def changedUpdate(e: DocumentEvent) { adjust() }
      def insertUpdate(e: DocumentEvent) { adjust() }
      def removeUpdate(e: DocumentEvent) { adjust() }

      private def adjust() { if (! isBlocked(a)) o(peer(a).getText) }
    }
}

// vim: set ts=2 sw=2 et:
