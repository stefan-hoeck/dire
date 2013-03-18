package dire.control

import dire.{Time, T0, Out}
import java.util.concurrent._, TimeUnit.{MILLISECONDS ⇒ MS}
import scalaz._, Scalaz._, effect.IO

object Clock {
  def apply(step: Time, out: Out[Time]): IO[IO[Unit]] = IO {
    var time = T0

    def clock = new Runnable{
      def run { time += step; out(time).unsafePerformIO }
    }

    val timerEx = Executors.newSingleThreadScheduledExecutor
    val handle = timerEx.scheduleAtFixedRate(clock, step, step, MS)
    
    IO {
      handle.cancel(true)
      timerEx.shutdownNow()
    }
  }
}

// vim: set ts=2 sw=2 et:
