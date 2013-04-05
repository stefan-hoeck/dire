package dire.swing

import dire._
import javax.swing.JLabel
import scalaz._, Scalaz._, effect.IO

case class Label(peer: JLabel) extends Component[JLabel] {
  def text: EF[Event[String],Nothing] = sink(this){ peer.setText }
}

object Label {
  def apply(): IO[Label] = apply("")

  def apply(text: String): IO[Label] =
    IO(Label(new JLabel(text)))

  implicit val LabelElem: AsSingleElem[Label] = Elem noFill { _.peer }
}

// vim: set ts=2 sw=2 et:
