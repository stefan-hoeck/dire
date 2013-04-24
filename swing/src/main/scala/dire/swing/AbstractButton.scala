package dire.swing

import dire._
import javax.swing.{AbstractButton ⇒ JAbstractButton, Icon}
import scalaz.effect.IO

trait AbstractButton[A]
  extends Component[A] 
  with IconDisplay[A]
  with TextDisplay[A] {
  protected def isBlocked(a: A): Boolean

  protected def setBlocked(a: A, b: Boolean) 

  def peer(a: A): JAbstractButton

  final def clicks(a: A): SIn[Unit] = SF cachedSrc a

  final def selected(a: A): Sink[Boolean] = sink{ b ⇒ 
    setBlocked(a, true)
    peer(a).setSelected(b)
    setBlocked(a, false)
  }

  final def value(a: A): SIn[Boolean] =
    clicks(a) map { _ ⇒ peer(a).isSelected } hold peer(a).isSelected

  final override def setHAlign(a: A, h: HAlign) =
    IO(peer(a).setHorizontalAlignment(h.v))

  final override def setHTextPos(a: A, h: HAlign) =
    IO(peer(a).setHorizontalTextPosition(h.v))

  final override def setText(a: A, s: String) = IO(peer(a).setText(s))

  final override def setVAlign(a: A, v: VAlign) =
    IO(peer(a).setVerticalAlignment(v.v))

  final override def setVTextPos(a: A, v: VAlign) =
    IO(peer(a).setVerticalTextPosition(v.v))

  final override def setDisabledIcon(a: A, i: Icon) =
    IO(peer(a).setDisabledIcon(i))
  
  final override def setIcon(a: A, i: Icon) = IO(peer(a).setIcon(i))

  final override def setIconTextGap(a: A, i: Int) =
    IO(peer(a).setIconTextGap(i))

  private implicit lazy val src: Source[A,Unit] =
    eventSrc { b ⇒ o ⇒ 
      val a = ali(_ ⇒ { if (! isBlocked(b)) o(()) })
      peer(b).addActionListener(a)
      _ ⇒ peer(b).removeActionListener(a)
    }
}

// vim: set ts=2 sw=2 et:
