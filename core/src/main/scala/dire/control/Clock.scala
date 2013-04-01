package dire.control

import dire.{Time, T0, Out}
import java.util.concurrent._, TimeUnit.{MICROSECONDS ⇒ MS}
import scalaz.effect.IO

/** An independent source of time events.
  *
  * This can be used to simulate asynchronous events being fired
  * at regular intervals
  */
private[dire] object Clock {

  def apply(start: Time, step: Time, out: Out[Time]): IO[IO[Unit]] = IO {
    var time = start

    def clock = new Runnable{
      def run { time += step; out(time).unsafePerformIO }
    }

    val timerEx = Executors.newSingleThreadScheduledExecutor
    val handle = timerEx.scheduleAtFixedRate(clock, step, step, MS)
    
    IO { handle.cancel(true); timerEx.shutdownNow() }
  }
}

// vim: set ts=2 sw=2 et:
