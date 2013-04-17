package dire.swing

import dire._
import javax.swing.JComboBox
import scala.reflect.runtime.universe.TypeTag
import scalaz._, Scalaz._, effect.IO

case class ComboBox[A:TypeTag](peer: JComboBox[A]) 
  extends Component[JComboBox[A]]
  with BlockedSignal {
  import ComboBox._

  def item: Sink[A] = itemO ∙ { _.some }
  
  def items[F[_]:Foldable](implicit T: TypeTag[F[A]]): Sink[F[A]] =
    blockedSink(this){ fa ⇒ 
      peer.removeAllItems()
      val as = fa.toList
      as foreach peer.addItem
      as.headOption foreach peer.setSelectedItem
    }

  def itemO: Sink[Option[A]] =
    blockedSink(this)(_ foreach peer.setSelectedItem)

  def value: SIn[A] = valueO collectO identity

  def valueO: SIn[Option[A]] =
    SF.cachedSrc[ComboBox[A],Option[A]](this)

  private def now: Option[A] = peer.getSelectedIndex match {
    case -1 ⇒ None
    case i  ⇒ Option(peer.getItemAt(i))
  }
}

object ComboBox {
  import scala.collection.JavaConversions._

  def apply[A:TypeTag,F[_]:Foldable](as: F[A]): IO[ComboBox[A]] = 
    IO(ComboBox[A](new JComboBox[A](new java.util.Vector[A](as.toList))))

  implicit def ComboBoxElem[A]: AsSingleElem[ComboBox[A]] =
    Elem hFill { _.peer }

  implicit def ComboBoxSource[A]: Source[ComboBox[A],Option[A]] =
    src[ComboBox[A],Option[A]](_.now) { c ⇒ o ⇒ 
      val a = ali(_ ⇒ { if (! c.blocked) o(c.now) })
      c.peer.addActionListener(a)
      _ ⇒ c.peer.removeActionListener(a)
    }
}

// vim: set ts=2 sw=2 et:
