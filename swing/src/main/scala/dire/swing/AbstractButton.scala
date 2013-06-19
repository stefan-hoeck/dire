package dire.swing

import dire._
import java.awt.event.ActionListener
import javax.swing.{AbstractButton ⇒ JAbstractButton, Icon, ButtonGroup}
import scalaz._, Scalaz._, effect.IO

trait AbstractButton[A]
  extends Component[A] 
  with IconDisplay[A]
  with TextDisplay[A] 
  with TextAlign[A]
  with Blockable[A] 
  with FiresActionEvent[A] 
  with IOWidget[A,Boolean] {

  def peer(a: A): JAbstractButton

  final override protected def addAli(a: A, ali: ActionListener) {
    peer(a).addActionListener(ali)
  }

  final override protected def removeAli(a: A, ali: ActionListener) {
    peer(a).removeActionListener(ali)
  }

  final def clicks(a: A): SIn[Unit] = actionEvents(a)

  protected def selOut(a: A): Out[Boolean] = b ⇒ 
    withBlockIO(a)(IO { peer(a).setSelected(b) })

  final def selected(a: A): Sink[Boolean] = sinkIO(selOut(a))

  final def out(a: A): Sink[Boolean] = selected(a)

  final def in(a: A): SIn[Boolean] =
    clicks(a) map { _ ⇒ peer(a).isSelected } hold peer(a).isSelected

  final def group[F[_]:Foldable,B:Equal](ps: F[(A,B)])
    : IO[SF[B,B]] = IO {
      val group = new ButtonGroup

      def sf(p: (A, B)): SF[B,B] = {
        val (a, b) = p
        val o = SF.id[B] collect { case x if b ≟ x ⇒ true } to selected(a)
        val i = in(a) filter identity as b

        o >> i
      }

      ps.toList foreach (p ⇒ group.add(peer(p._1)))

      ps foldMap sf
    }

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
}

object AbstractButton {
  implicit val AbstractButtonIso = new (AbstractButton ~>> JAbstractButton) {
    def apply[A](f: AbstractButton[A], a: A) = f peer a
  }
}

// vim: set ts=2 sw=2 et:
