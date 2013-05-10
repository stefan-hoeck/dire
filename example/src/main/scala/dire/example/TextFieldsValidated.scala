package dire.example

import dire._
import dire.swing._, Swing._
import scalaz._, Scalaz._

object TextFieldsValidated extends SwingApp {
  type ValRes[+A] = Validation[NonEmptyList[String],A]
  type VSIn[+A] = SIn[ValRes[A]]

  implicit val VSInApplicative = Applicative[SIn].compose[ValRes]

  override def behavior(f: Frame) = for {
    first  ← TextField(columns := 50)
    last   ← TextField()
    btn    ← Button(text := "Set name")
    full   ← Label()
    acc    ← Label()
    msg    ← Label(foreground := java.awt.Color.RED)

    //Layout (available through class Elem and its implicit syntax classes)
    _ ← ("First name" beside first) above
        ("Last name" beside last) above
        ("Full name" beside full) above
        ("Accepted name" beside acc) above
        ("Error:" beside msg) above
        (btn fillH 2) addTo f

    //Validate first and last name and concatenate them
    fullV = ^(validate("First name")(first.textIn),
              validate("Last name")(last.textIn)){ _ + " " + _ }

    sf = fullV.branch(enable(btn)) //disable btn if invalid
              .branch(printError(msg)) //display error message if invalid
              .andThen(display(full)) //print to full name label if valid
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
  private def enable(b: Button) = id map { _.isSuccess } to b.enabled

  //displays error message if invalid
  private def printError(l: Label) = 
    id map { _.fold(_.head, _ ⇒ "") } to l.text

  //collects valid inputs and displays them in label
  private def display(l: Label) = id.collectS to l.text
}

// vim: set ts=2 sw=2 et:
