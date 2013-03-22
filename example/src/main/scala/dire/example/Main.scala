package dire.example

import dire.{SF, Time, T0, Event}, SF.EventsOps
import java.util.concurrent._
import scalaz._, Scalaz._, effect._

object Main extends SafeApp {

  def timeOut = SF.time --> { t ⇒ IO.putStrLn(s"Tick at $t microseconds") }

  override def runc: IO[Unit] = SF.runReactive(timeOut)(_ >= 2000000L)
}
