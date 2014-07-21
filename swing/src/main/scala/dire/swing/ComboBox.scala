package dire.swing

import dire._
import javax.swing.JComboBox
import scalaz._, Scalaz._, effect.IO

case class ComboBox[A](peer: JComboBox[A]) {
  private var blocked = false
}

object ComboBox {
  def apply[A](items: List[A])(props: (ComboBox[A] ⇒ IO[Unit])*)
    : IO[ComboBox[A]] = for {
      res ← IO(new ComboBox[A](new JComboBox[A]()))
      _   ← Swing.items[A,List].:=(items)(res)(ComboBoxComponent[A])
      _   ← props.toList foldMap { _(res) }
    } yield res

  def apply[A](items: List[A], item: A): IO[ComboBox[A]] =
    apply[A](items)(Swing.item[A] := item)

  implicit def ComboBoxComponent[A] =
    new ComboBoxLike[ComboBox[A],A] {
      def peer(b: ComboBox[A]) = b.peer
      protected def isBlocked(a: ComboBox[A]) = a.blocked
      protected def block(a: ComboBox[A], b: Boolean) { a.blocked = b } 
    }

  implicit def ComboBoxElem[A]: AsSingleElem[ComboBox[A]] =
    Elem hFill { _.peer }
}

// vim: set ts=2 sw=2 et:
