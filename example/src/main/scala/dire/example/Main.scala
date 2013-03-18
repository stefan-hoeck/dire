package dire.example

import dire.control.{Source, Reactor}
import java.util.concurrent._
import scalaz._, Scalaz._, effect._
import scalaz.concurrent.Strategy.Executor

object Main extends SafeApp {

  implicit val s: ExecutorService = Executors.newFixedThreadPool(4)

  val is = List(230, 150, 2343, 321, 120, 450, 771)

  override def runc: IO[Unit] = for {
    ss   ← is traverse { Source ticker _ }
    kill ← dire.control.Reactor.run(ss, 1)(Executor(s))
    _    ← IO(Thread.sleep(30000))
    _    ← kill
    _    ← IO(Thread.sleep(10))
    _    ← IO(s.shutdown)
  } yield ()
}
