package dire.swing

import dire._
import java.awt.Color
import javax.swing.text.{JTextComponent, Caret}
import javax.swing.event.{DocumentListener, DocumentEvent}
import scalaz._, Scalaz._, effect.IO

trait TextComponent[A]
  extends TextDisplay[A]
  with Blockable[A] 
  with IOWidget[A,String] {
  final def caret(a: A): Sink[Caret] = sink(peer(a).setCaret)

  final def caretColor(a: A): Sink[Color] = sink(peer(a).setCaretColor)

  final def caretPosition(a: A): Sink[Int] = sink(peer(a).setCaretPosition)

  final def disabledTextColor(a: A): Sink[Color] =
    sink(peer(a).setDisabledTextColor)

  final def editable(a: A): Sink[Boolean] = sink(peer(a).setEditable)

  def peer(a: A): JTextComponent

  final def selectedTextColor(a: A): Sink[Color] =
    sink(peer(a).setSelectedTextColor)

  final def selectionColor(a: A): Sink[Color] = sink(peer(a).setSelectionColor)

  final def selectionEnd(a: A): Sink[Int] = sink(peer(a).setSelectionEnd)

  final def selectionStart(a: A): Sink[Int] = sink(peer(a).setSelectionStart)

  final def setText(a: A, s: String) = IO(peer(a).setText(s))

  final def in(a: A): SIn[String] = SF cachedSrc a

  final def out(a: A): Sink[String] = text(a)

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

object TextComponent {
  def apply[A:TextComponent]: TextComponent[A] = implicitly

  implicit val TextComponentIso = new (TextComponent ~>> JTextComponent) {
    def apply[A](f: TextComponent[A], a: A) = f peer a
  }
}

// vim: set ts=2 sw=2 et:
