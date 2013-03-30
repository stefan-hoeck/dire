package dire.example

import dire.{SF, Time, T0, Event}, SF.EventsOps
import java.util.concurrent._
import scalaz._, Scalaz._, effect._

object Main extends SafeApp {
  override def runc: IO[Unit] = ButtonApp.runc
}
