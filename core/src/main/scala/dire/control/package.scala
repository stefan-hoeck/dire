package dire

import java.util.concurrent.CountDownLatch

/** Contains classes and logic for the inner workings of the reactive
  * framework.
  *
  * Classes and methods in this package are typically not referentially
  * transparent and can therefore not be accessed or manipulated from
  * outside the reactive framework.
  */
package object control {
  /** An impure data sink */
  private[dire] type Sink[-A] = A ⇒ Unit

  private[control] def await[A](cnt: Int, f: CountDownLatch ⇒ A): A = {
    val cdl = new CountDownLatch(cnt)
    val res = f(cdl)
    cdl.await()
    res
  }
}

// vim: set ts=2 sw=2 et:
