package dire.swing

import dire._
import javax.swing.{AbstractButton ⇒ JAbstractButton}

trait AbstractButton[A<:JAbstractButton]
  extends Component[A]
  with BlockedSignal {
  import AbstractButton._

  def clicks: SIn[Unit] = SF cachedSrc this

  def value: SIn[Boolean] =
    clicks map { _ ⇒ peer.isSelected } hold peer.isSelected

  def selected: Sink[Boolean] =
    blockedSink(this)(peer.setSelected(_))

  def text: Sink[String] = sink(peer.setText, this)
}

object AbstractButton {

  implicit def ButtonSource[A<:JAbstractButton]
    : Source[AbstractButton[A],Unit] = eventSrc { b ⇒ o ⇒ 
    val a = ali(_ ⇒ { b.blocked = true; o(()); b.blocked = false })
    b.peer.addActionListener(a)
    _ ⇒ b.peer.removeActionListener(a)
  }
}

// vim: set ts=2 sw=2 et:
