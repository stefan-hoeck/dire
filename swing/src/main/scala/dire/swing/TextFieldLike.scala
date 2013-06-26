package dire.swing

import dire._
import java.awt.event.ActionListener
import javax.swing.JTextField
import scalaz._, Scalaz._, effect.IO

/** Type class representing JTextField and its subclasses */
trait TextFieldLike[A]
  extends TextComponent[A]
  with TextAlign[A] 
  with FiresActionEvent[A] {
  def peer(a: A): JTextField

  final override protected def addAli(a: A, ali: ActionListener) {
    peer(a).addActionListener(ali)
  }

  final override protected def removeAli(a: A, ali: ActionListener) {
    peer(a).removeActionListener(ali)
  }

  final override def setHAlign(a: A, h: HAlign) =
    IO(peer(a).setHorizontalAlignment(h.v))

  final def columns(a: A): Sink[Int] = sink(peer(a).setColumns)

  final def textEvents(a: A): SIn[String] = 
    in(a) on actionEvents(a)

  final def sfE(a: A): SF[String,String] = 
    SF.id.to(text(a)) >> textEvents(a)

  override def setHTextPos(a: A, h: HAlign) = IO.ioUnit
  override def setVAlign(a: A, v: VAlign) = IO.ioUnit
  override def setVTextPos(a: A, v: VAlign) = IO.ioUnit
}

object TextFieldLike {
  def apply[A:TextFieldLike]: TextFieldLike[A] = implicitly

  implicit val TextFieldLikeIso = new (TextFieldLike ~>> JTextField) {
    def apply[A](f: TextFieldLike[A], a: A) = f peer a
  }
}
// vim: set ts=2 sw=2 et:
