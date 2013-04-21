package dire.control

import dire.{SF, SIn, Time}
import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.{Future, Await, duration}, duration.Duration.Inf
import scalaz._, Scalaz._, effect.IO
import scalaz.concurrent.Strategy

final class ReactiveSystem private(
    ex: ExecutorService,
    killers: Var[Option[Unit]]) {

  private val s = Strategy.Executor(ex)

  /** Same as `SF.run` but does not block the calling thread */
  def runAsync[A](in: SIn[A],
                  proc: Int = SF.processors,
                  step: Time = 1000L)
                 (stop: A ⇒ Boolean): IO[Unit] =
    async(SF.run(in, proc, step)(stop)).void

  /** Ansynchronously starts a reactive graph and runs it until the
    * returned IO action is executed.
    */
  def forever[A](in: SIn[A],
                 proc: Int = SF.processors,
                 step: Time = 1000L): IO[IO[Unit]] = for {
    v    ← Var newVar none[Unit]
    f    ← async(SF.run(in >> SF.src(v), proc, step)(_.nonEmpty))
    kill = (s: Option[Unit]) ⇒ s.cata(
             _ ⇒ v.put(s) >> IO{ v.shutdown(); Await.ready(f, Inf); () },
             IO.ioUnit
           )
    _    ← IO(killers.addListener(kill))
  } yield kill(().some) >>
          IO { killers.removeListener(kill) }

  def shutdown: IO[Unit] =
    killers.put(().some) >> IO { ex.shutdown() }

  private def async(io: IO[Unit]): IO[Future[Unit]] = IO {
    import scala.concurrent.{ExecutionContext}
    implicit val context = ExecutionContext.fromExecutor(ex)

    val f = Future(io.unsafePerformIO)
    f onComplete { _ ⇒ ex.shutdown() }

    f
  }
}

object ReactiveSystem {
  def apply(): IO[ReactiveSystem] = for {
    ex ← IO(Executors.newCachedThreadPool())
    v  ← Var newVar none[Unit]
  } yield new ReactiveSystem(ex, v)
}

// vim: set ts=2 sw=2 et:
