package dire.swing

trait BlockedSignal {
  private[swing] var blocked = false

  private[swing] def withBlock(f: ⇒ Unit) {
    blocked = true
    val x = f
    blocked = false
  }
}

// vim: set ts=2 sw=2 et:
