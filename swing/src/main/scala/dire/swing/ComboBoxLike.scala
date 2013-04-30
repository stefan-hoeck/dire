package dire.swing

import dire._
import javax.swing.JComboBox
import scala.reflect.runtime.universe.TypeTag
import scalaz._, Scalaz._, effect.IO

trait ComboBoxLike[A,B]
  extends Component[A] 
  with Blockable[A]
  with IOWidget[A,B] 
  with Editable[A] {
  implicit def T: TypeTag[B]

  def peer(a: A): JComboBox[B]

  final def in(a: A): SIn[B] = itemInO(a) collectO identity

  final def item(a: A): Sink[B] = itemO(a) ∙ { _.some }

  final def itemO(a: A): Sink[Option[B]] = sink { ob ⇒ 
    withBlock(a) { peer(a).setSelectedItem(ob getOrElse null) }
  }
  
  final def items[F[_]:Foldable](a: A): Sink[F[B]] = sinkIO(setItems(a,_))

  final def itemInO(a: A): SIn[Option[B]] = SF.cachedSrc[A,Option[B]](a)

  final def out(a: A): Sink[B] = item(a)

  final def setEditable(a: A, b: Boolean) = IO(peer(a).setEditable(b))

  final def setItems[F[_]:Foldable](a: A, fb: F[B]): IO[Unit] = IO {
    withBlock(a) {
      peer(a).removeAllItems()
      val bs = fb.toList
      bs foreach peer(a).addItem
      bs.headOption foreach peer(a).setSelectedItem
    }
  }

  private def now(a: A): Option[B] = peer(a).getSelectedIndex match {
    case -1 ⇒ None
    case i  ⇒ Option(peer(a) getItemAt i)
  }

  private def textNow(a: A): Option[String] = 
    Option(peer(a).getSelectedItem) ∘ { _.toString }

  private implicit def source: Source[A,Option[B]] =
    src[A,Option[B]](now) { c ⇒ o ⇒ 
      val a = ali(_ ⇒ { if (! isBlocked(c)) o(now(c)) })
      peer(c).addActionListener(a)
      _ ⇒ peer(c).removeActionListener(a)
    }
}

object ComboBoxLike {
  def apply[A,B](implicit W: ComboBoxLike[A,B]): ComboBoxLike[A,B] = W

  implicit def ComboBoxIso[B] =
    new (({type λ[α]=ComboBoxLike[α,B]})#λ ~>> JComboBox[B]) {
      def apply[A](f: ComboBoxLike[A,B], a: A) = f peer a
    }
}

// vim: set ts=2 sw=2 et:
