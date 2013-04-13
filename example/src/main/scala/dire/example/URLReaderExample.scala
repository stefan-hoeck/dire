package dire.example

import dire._
import dire.swing._, Frame.North, Elem._
import scalaz._, Scalaz._, effect.IO
import scala.io.Source
import scala.xml.XML
import util.control.NonFatal

object URLReaderExample extends SwingApp with SFInstances {
  type Coords = (Double, Double)

  override def behavior(f: Frame) = for {
    city        ← TextField()
    set         ← Button("Set")
    coordinates ← Label()
    cityTime    ← Label()
    utcTime     ← Label()
    diffTime    ← Label()

    //Layout (available through class Elem and its implicit syntax classes)
    _ ← ("City" beside city beside set) above
        ("Coordinates" beside coordinates) above
        ("City time" beside cityTime) above
        ("UTC time" beside utcTime) above
        ("Difference" beside diffTime) prefWidth 500 addToFrame (f, North)

    coords = (city.value on set.clicks) andThen
             (SF sfIO readCoords hold (0D, 0D) to coordinates.textA)
    utc = utcIn to utcTime.textA
    ct  = cityIn(coords) to cityTime.textA
    sf  = (utc ⊛ ct)(diff) to diffTime.textA
  } yield sf

  //Fires an event every second
  val ticks = SF ticks 1000000L

  //Reads the actual time in a city (given as a string signal) every second
  def cityIn(c: SIn[Coords]) = c on ticks andThen SF.sfIO(readCity)

  //Reads the actual UTC time every second
  val utcIn = ticks andThen SF.sfIO(_ ⇒ readUTC)

  // *** Business Logic: Reading stuff from URLs *** //

  def diff(a: HMTime, b: HMTime) = HMTime(a.h - b.h, a.m - b.m, a.s - b.s)

  case class HMTime(h: Int, m: Int, s: Int) {
    override def toString = f"$h%02d : $m%02d : $s%02d"
  }

  val HMZero = HMTime(0, 0, 0)

  final val TimeString = """\d\d? ... .... (\d\d):(\d\d):(\d\d)""".r

  def readCity(coords: Coords): IO[HMTime] = IO { coords match {
    case (lat, lon) ⇒ {
      val url = s"http://www.earthtools.org/timezone/${lat}/${lon}"
      (XML.loadString(Source.fromURL(url, "UTF-8").mkString) \ "localtime").text match {
        case TimeString(h, m, s) ⇒ HMTime(h.toInt, m.toInt, s.toInt)
        case s                   ⇒ println(s); HMZero
      }
    }
  }} except { t ⇒ IO.putStrLn(t.toString) as HMZero }

  def readUTC = readCity(52.0, 0.0)

  def readCoords(city: String): IO[Coords] = IO {
    val url = s"http://nominatim.openstreetmap.org/search?q=${city}&format=xml"
    val fst = XML.loadString(Source.fromURL(url, "UTF-8").mkString) \ "place" head

    val lat = (fst \ "@lat" text).toDouble
    val lon = (fst \ "@lon" text).toDouble
    (lat, lon)
  } except { t ⇒ IO.putStrLn(t.toString) as (0.0, 0.0) }
}

// vim: set ts=2 sw=2 nowrap et:
