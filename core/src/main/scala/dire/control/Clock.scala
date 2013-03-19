package dire.control

import dire.{Time, T0}
import java.util.concurrent._, TimeUnit.{MICROSECONDS ⇒ MS}

private[control] object Clock {

  def apply(step: Time, out: Time ⇒ Unit): () ⇒ Unit = {
    var time = T0

    def clock = new Runnable{
      def run { time += step; out(time) }
    }

    val timerEx = Executors.newSingleThreadScheduledExecutor
    val handle = timerEx.scheduleAtFixedRate(clock, step, step, MS)
    
    () ⇒ { handle.cancel(true); timerEx.shutdownNow() }
  }

}

// vim: set ts=2 sw=2 et:
