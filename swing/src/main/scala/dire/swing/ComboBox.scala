package dire.swing

import dire._
import javax.swing.JComboBox
import scala.reflect.runtime.universe.TypeTag
import scalaz._, Scalaz._, effect.IO

case class ComboBox[A](peer: JComboBox[A]) {
  private var blocked = false
}

object ComboBox {
  def apply[A:TypeTag](items: List[A], props: ComboBox[A] ⇒ IO[Unit]*)
    : IO[ComboBox[A]] = for {
      res ← IO(new ComboBox[A](new JComboBox[A]()))
      _   ← Swing.items[A,List].:=(items)(res)(ComboBoxComponent[A])
      _   ← props.toList foldMap { _(res) }
    } yield res

  implicit def ComboBoxComponent[A](implicit W: TypeTag[A]) =
    new ComboBoxLike[ComboBox[A],A] {
      override val T = W
      def peer(b: ComboBox[A]) = b.peer
      protected def isBlocked(a: ComboBox[A]) = a.blocked
      protected def block(a: ComboBox[A], b: Boolean) { a.blocked = b } 
    }

  implicit def ComboBoxElem[A]: AsSingleElem[ComboBox[A]] =
    Elem hFill { _.peer }
}

// vim: set ts=2 sw=2 et:
