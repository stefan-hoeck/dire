package dire.example

import dire._
import dire.swing._, Frame.North, Elem._
import scalaz._, Scalaz._

object TextFieldsValidated extends SwingApp {
  type ValRes[+A] = Validation[NonEmptyList[String],A]
  type VSIn[+A] = SIn[ValRes[A]]

  implicit val VSInApplicative = Applicative[SIn].compose[ValRes]

  override def behavior(f: Frame) = for {
    first  ← TextField()
    last   ← TextField()
    btn    ← Button("Set name")
    full   ← Label()
    acc    ← Label()
    msg    ← Label()

    //Layout (available through class Elem and its implicit syntax classes)
    _ ← ("First name" beside first) above
        ("Last name" beside last) above
        ("Full name" beside full) above
        ("Accepted name" beside acc) above
        ("Error:" beside msg) above
        (btn fillH 2) prefWidth 500 addToFrame (f, North)

    //Validate first and last name and concatenate them
    fullV = ^(validate("First name")(first.value),
              validate("Last name")(last.value)){ _ + " " + _ }

    sf = fullV.to(enable(btn)) //disable btn if invalid
              .to(printError(msg)) //display error message if invalid
              .andThen(display(full)) //print to full name label if valid
              .hold("") //event stream of strings to signal of strings
              .on(btn.clicks) //fires signal's actual value if btn is clicked
              .to(acc.text) //display events in label acc
  } yield sf

  
  private def validate(prop: String)(sin: SIn[String]): VSIn[String] =
    sin map { s ⇒
      if (s.isEmpty) s"$prop must not be an empty string".failureNel
      else s.success
    }

  private val id = SF.id[ValRes[String]]

  //disables button if input is invalid
  private def enable(b: Button) = id mapS { _.isSuccess } toE b.enabled

  //displays error message if invalid
  private def printError(l: Label) = 
    id mapS { _.fold(_.head, _ ⇒ "") } toE l.text

  //collects valid inputs and displays them in label
  private def display(l: Label) = id.ef collect { _.toOption } to l.text
}

// vim: set ts=2 sw=2 et:
