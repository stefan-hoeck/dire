package dire.swing

import dire._
import javax.swing.JLabel
import scalaz._, Scalaz._, effect.IO

case class Label(peer: JLabel) extends Component[JLabel] {
  def text: Sink[String] = sink(peer.setText, this)

  def textA[A]: Sink[A] = text ∙ { _.toString }

  def textS[A:Show]: Sink[A] = text ∙ { _.shows }
}

object Label {
  def apply(): IO[Label] = apply("")

  def apply(text: String): IO[Label] =
    IO(Label(new JLabel(text)))

  implicit val LabelElem: AsSingleElem[Label] = Elem noFill { _.peer }
}

// vim: set ts=2 sw=2 et:
