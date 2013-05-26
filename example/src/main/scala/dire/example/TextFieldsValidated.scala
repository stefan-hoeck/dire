package dire.example

import dire._, validation._
import dire.swing._, Swing._
import scalaz._, Scalaz._

object TextFieldsValidated extends SwingApp {

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

    handleV = (btn.enabled ∙ isSuccess) ⊹ (msg.text ∙ errorMsg)

    successes = fullV to handleV collectS

    sf = successes to full.text on btn.clicks to acc.text
  } yield sf

  
  private def validate(prop: String)(sin: SIn[String]): VSIn[String] =
    sin map { s ⇒
      if (s.isEmpty) s"$prop must not be an empty string".failureNel
      else s.success
    }

  private def errorMsg(v: ValRes[String]) = v fold (_.head, _ ⇒ "")

  private def isSuccess(v: ValRes[String]) = v.isSuccess
}

// vim: set ts=2 sw=2 et:
