package dire.example

import dire.EF
import java.util.concurrent._
import scalaz._, Scalaz._, effect._

object Main extends SafeApp {

  val timesOut = EF.times
                   .filter(_ % 200000L == 0L)
                   .to(t ⇒ IO.putStrLn(s"Tick at $t microseconds"))

  override def runc: IO[Unit] = for {
    kill ← EF.runReactive(timesOut)
    _    ← IO(Thread.sleep(10000))
    _    ← kill
  } yield ()
}
