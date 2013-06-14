package dire.control

import dire.{SF, SIn, Time, DataSource}
import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.{Future, Await, duration}, duration._
import scalaz._, Scalaz._, effect.IO
import scalaz.concurrent.Strategy

final class ReactiveSystem private(
    ex: ExecutorService,
    killers: Var[Boolean],
    log: ⇒ String ⇒ IO[Unit]) {

  private val s = Strategy.Executor(ex)

  /** Same as `SF.run` but does not block the calling thread */
  def runAsync[A](in: SIn[A],
                  step: Time = 1000L)
                 (stop: A ⇒ Boolean): IO[Unit] =
    async(SF.runS(in, s, step)(stop)).void

  /** Ansynchronously starts a reactive graph and runs it until the
    * returned IO action is executed.
    */
  def forever[A](in: SIn[A],
                 step: Time = 1000L): IO[IO[Unit]] = for {
    _    ← log(s"Starting $in to run forever")
    v    ← Var newVar false
    f    ← async(SF.runS(in >> killerIn(v), s, step)(identity))
    kill = killer(v, f, in)
    _    ← IO(killers.addListener(kill))
  } yield IO { killers.removeListener(kill) } >> kill(true)

  def shutdown: IO[Unit] =
    killers.put(true) >>
    IO(killers.shutdown()) >>
    log(s"Shutting down executor of reactive system $this") >>
    IO { ex.shutdown() } >>
    log(s"Reactive system $this shut down")

  private def async(io: IO[Unit]): IO[Future[Unit]] = IO {
    import scala.concurrent.{ExecutionContext}
    implicit val context = ExecutionContext.fromExecutor(ex)

    Future(io.unsafePerformIO)
  }

  private def await[A](f: Future[A], sf: AnyRef): IO[Unit] = for {
    _ ← log(s"Initialized shutdown for $sf")
    b ← IO {
          try { Await.ready(f, 5 second); false } catch {
            case t: java.util.concurrent.TimeoutException ⇒ true
          }
        }
    _ ← b ? log(s"Awaiting shutdown for $sf timed out") | IO.ioUnit
  } yield ()

  private def killer[A](v: Var[Boolean], f: Future[A], sf: AnyRef) =
    (b: Boolean) ⇒ 
      if (b) v.put(b) >> await(f, sf) >> IO(v.shutdown())
      else IO.ioUnit

  private def killerIn(v: Var[Boolean]): SIn[Boolean] = {
    val src = DataSource.eventSrc[Var[Boolean],Boolean](
      s ⇒ o ⇒ IO(s.addListener(o)) as IO.ioUnit)

    SF.src(v)(src)
  }
}

object ReactiveSystem {
  def apply(log: String ⇒ IO[Unit] = (s: String) ⇒ IO.putStrLn(s))
    : IO[ReactiveSystem] = for {
      ex ← IO(Executors.newCachedThreadPool())
      v  ← Var newVar false
      res = new ReactiveSystem(ex, v, log)
      _  ← log(s"Reactive system started: $res")
    } yield res
}

// vim: set ts=2 sw=2 et:
