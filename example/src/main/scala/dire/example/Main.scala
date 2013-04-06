package dire.example

import scalaz.effect.{IO, SafeApp}

object Main extends SafeApp {
  override def runc: IO[Unit] = FormattedIntegers.run
}
