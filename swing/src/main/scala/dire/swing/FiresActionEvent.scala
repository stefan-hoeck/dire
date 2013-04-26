package dire.swing

import dire._
import java.awt.event.ActionListener

trait FiresActionEvent[A] extends Blockable[A] {
  protected def addAli(a: A, ali: ActionListener): Unit
  protected def removeAli(a: A, ali: ActionListener): Unit

  final def actionEvents(a: A): SIn[Unit] = SF cachedSrc a

  private implicit lazy val src: Source[A,Unit] =
    eventSrc { b ⇒ o ⇒ 
      val a = ali(_ ⇒ { if (! isBlocked(b)) o(()) })
      addAli(b,a)
      _ ⇒ removeAli(b, a)
    }
}

// vim: set ts=2 sw=2 et:
