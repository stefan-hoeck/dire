package dire.example

//import dire._
//import dire.swing._, Frame.North, Elem._
//import java.lang.{Integer ⇒ JInt}
//import scalaz._, Scalaz._
//
///** Reads an integer in different formats from text fields
//  * and displays the results in the same text fields.
//  *
//  * Note how adding additional input and output devices is trivial:
//  * both input (an event stream function) and output
//  * (again an event function: The result is always the empty event stream)
//  * define Monoids.
//  * The greatest complexity comes from input validation and displaying
//  * error messages.
//  */
//object FormattedIntegers extends SwingApp {
//  type ValRes[+A] = Validation[NonEmptyList[String],A]
//
//  override def behavior(f: Frame) = for {
//    bin  ← TextField("0")
//    hex  ← TextField("0")
//    dec  ← TextField("0")
//    msg  ← Label()
//
//    //Layout (available through class Elem and its implicit syntax classes)
//    _ ← ("Binary" beside bin) above
//        ("Decimal" beside dec) above
//        ("Hexadecimal" beside hex) above
//        ("Error:" beside msg) prefWidth 500 addToFrame (f, North)
//
//    //Combined validated input events
//    input = (dec.value map parse(10, "decimal")) ⊹ 
//            (hex.value map parse(16, "hexadecimal")) ⊹ 
//            (bin.value map parse(2, "binary"))
//
//    //Errors to label and then collect successes only; start with 0
//    validInput = input branch printError(msg) collectO { _.toOption }
//
//    //Combined output of valid integers
//    output = (toDec to dec.text) ⊹ 
//             (toHex to hex.text) ⊹ 
//             (toBin to bin.text)
//
//  } yield validInput andThen output
//
//  private def toDec = SF.id[Int] map { _.toString }
//
//  private def toBin = SF.id[Int] map JInt.toBinaryString
//
//  private def toHex = SF.id[Int] map JInt.toHexString
//
//  private def parse(base: Int, name: String)(s: String): ValRes[Int] = try {
//    JInt.parseInt(s, base).success
//  } catch {
//    case util.control.NonFatal(_) ⇒ s"Not a $name integer: $s".failureNel
//  }
//    
//  //displays error message if invalid
//  private def printError(l: Label) = 
//    SF.id[ValRes[Int]] map { _.fold(_.head, _ ⇒ "") } to l.text
//}

// vim: set ts=2 sw=2 et:
