package dire.example

import dire.{SF, Time, T0, Event}, SF.EventsOps
import java.util.concurrent._
import scalaz._, Scalaz._, effect._

object Main extends SafeApp {

  def timeOut = SF.time
                  .changes[Time]
                  .filter { _ % 200000L == 0L }
                  .eventTo { t ⇒ IO.putStrLn(s"Tick at $t microseconds") }

  override def runc: IO[Unit] =
    SF.runReactive[Event[Time]](timeOut, _ fold (_ >= 20000000L, false))
}
