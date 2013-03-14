package dire

/** Highly inefficient model implementation */
package object model {
  type Nel[+A] = scalaz.NonEmptyList[A]

  def nel[A](a: A, as: List[A]) = scalaz.NonEmptyList[A](a, as: _*)
}

// vim: set ts=2 sw=2 et:
