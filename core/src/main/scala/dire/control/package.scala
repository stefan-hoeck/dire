package dire

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
}

// vim: set ts=2 sw=2 et:
