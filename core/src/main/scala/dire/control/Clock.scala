package dire.control

import dire.{Time, T0}
import java.util.concurrent._, TimeUnit.{MICROSECONDS ⇒ MS}

/** An independent source of time events.
  *
  * This can be used to simulate asynchronous events being fired
  * at regular intervals
  */
private[dire] object Clock {

  def apply(start: Time, step: Time, out: Time ⇒ Unit): Sink[Unit] = {
    var time = start

    def clock = new Runnable{
      def run { time += step; out(time) }
    }

    val timerEx = Executors.newSingleThreadScheduledExecutor
    val handle = timerEx.scheduleAtFixedRate(clock, step, step, MS)
    
    _ ⇒ { handle.cancel(true); timerEx.shutdownNow() }
  }
}

// vim: set ts=2 sw=2 et:
