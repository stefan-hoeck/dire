package dire.example

import dire._, SF.EFOps
import dire.swing._, Frame.North, Elem._
import scalaz._, Scalaz._, effect.IO
import scala.io.Source
import scala.xml.XML

object URLReaderExample extends SwingApp {
  override def behavior(f: Frame) = for {
    city      ← TextField("LONDON")
    cityTime  ← Label()
    utcTime   ← Label()
    diffTime  ← Label()

    //Layout (available through class Elem and its implicit syntax classes)
    _ ← ("City" beside city) above
        ("City time" beside cityTime) above
        ("UTC time" beside utcTime) above
        ("Difference" beside diffTime) prefWidth 300 addToFrame (f, North)

    utc = utcIn to utcTime.textA hold HMTime(0, 0)
    ct  = cityIn(city.value) to cityTime.textA hold HMTime(0, 0)
    sf  = (utc ⊛ ct)(diff) toE diffTime.textA
  } yield sf

  //Fires an event every second
  val ticks = EF ticks 1000000L

  //Reads the actual time in a city (given as a string signal) every second
  def cityIn(c: SIn[String]) = c on ticks andThen EF(readCity)

  //Reads the actual UTC time every second
  val utcIn = ticks andThen EF(_ ⇒ readUTC)

  // *** Business Logic *** //

  def diff(a: HMTime, b: HMTime) = HMTime(a.h - b.h, a.m - b.m)

  case class HMTime(h: Int, m: Int) {
    override def toString = f"$h%02d : $m%02d"
  }

  final val URL = 
    """ http://www.nanonull.com/TimeService/TimeService.asmx/"""

  final val UTC = URL + "getUTCTime"

  final val AMString = """(\d\d?):(\d\d) AM""".r
  final val PMString = """(\d\d?):(\d\d) PM""".r

  def readCity(c: String) = read(URL + s"getCityTime?city=$c")

  def readUTC = read(UTC)

  def read(url: String): IO[HMTime] = IO {
    try {
      XML.loadString(Source.fromURL(url, "UTF-8").mkString).text match {
        case AMString(h, m) ⇒ HMTime(h.toInt, m.toInt)
        case PMString(h, m) ⇒ HMTime(h.toInt + 12, m.toInt)
        case x              ⇒ HMTime(0, 0)
      }
    } catch { case util.control.NonFatal(e) ⇒ HMTime(0, 0) }
  }
}

// vim: set ts=2 sw=2 nowrap et:
