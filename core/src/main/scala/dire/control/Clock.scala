package dire.control

import dire.{Time, T0}
import java.util.concurrent._, TimeUnit.{MICROSECONDS ⇒ MS}

private[control] object Clock {

  def apply(step: Time, out: Time ⇒ Unit): Sink[CountDownLatch] = {
    var time = T0

    def clock = new Runnable{
      def run { time += step; out(time) }
    }

    val timerEx = Executors.newSingleThreadScheduledExecutor
    val handle = timerEx.scheduleAtFixedRate(clock, step, step, MS)
    
    cdl ⇒ {
      handle.cancel(true)
      timerEx.shutdownNow()
      cdl.countDown()
    }
  }

}

// vim: set ts=2 sw=2 et:
