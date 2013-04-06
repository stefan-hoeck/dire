package dire.example

import dire._
import dire.swing._, Frame.North, Elem._
import java.lang.{Integer ⇒ JInt}
import scalaz._, Scalaz._

/** Reads an integer in different formats from text fields
  * and displays the results in the same text fields.
  *
  * Note how adding additional input and output devices is trivial:
  * both input (an event stream function) and output
  * (again an event function: The result is always the empty event stream)
  * define Monoids.
  * The greatest complexity comes from input validation and displaying
  * error messages.
  */
object FormattedIntegers extends SwingApp {
  type ValRes[+A] = Validation[NonEmptyList[String],A]

  override def behavior(f: Frame) = for {
    bin  ← TextField()
    hex  ← TextField()
    dec  ← TextField()
    msg  ← Label()

    //Layout (available through class Elem and its implicit syntax classes)
    _ ← ("Binary" beside bin) above
        ("Decimal" beside dec) above
        ("Hexadecimal" beside hex) above
        ("Error:" beside msg) prefWidth 500 addToFrame (f, North)

    //Combined validated input events
    input = (dec.value map parse(10, "decimal") events) ⊹ 
            (hex.value map parse(16, "hexadecimal") events) ⊹ 
            (bin.value map parse(2, "binary") events)

    //Errors to label and then collect successes only; start with 0
    validInput =
      (input hold 0.success).ef to printError(msg) collect { _.toOption }

    //Combined output of valid integers
    output = (toDec andThen dec.text) ⊹ 
             (toHex andThen hex.text) ⊹ 
             (toBin andThen bin.text)

  } yield validInput andThen output

  private def toDec = EF.id[Int] map { _.toString }

  private def toBin = EF.id[Int] map JInt.toBinaryString

  private def toHex = EF.id[Int] map JInt.toHexString

  private def parse(base: Int, name: String)(s: String): ValRes[Int] = try {
    JInt.parseInt(s, base).success
  } catch {
    case util.control.NonFatal(_) ⇒ s"Not a $name integer: $s".failureNel
  }
    
  //displays error message if invalid
  private def printError(l: Label) = 
    EF.id[ValRes[Int]] mapE { _.fold(_.head, _ ⇒ "") } to l.text
}

// vim: set ts=2 sw=2 et:
