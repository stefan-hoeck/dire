package dire.control

import collection.mutable.ListBuffer
import dire.Time

/** `Node`s are used to set up the dependency graph between
  * reactive components. They should be an implementation
  * detail invisible to clients of the library.
  *
  * If a reactive component fires an event, its node and all
  * child nodes are marked as dirty, meaning they need to be
  * reevaluated. The event firing node then asks all child
  * nodes to update their state. A node is only recalculated,
  * if none of its parents are still dirty. This guarantees,
  * that during a single reevaluation cycle each dirty node is
  * recalculated exactly once. During reevaluation, all nodes
  * set their state back to non-dirty. After that follows
  * the cleanup phase, during which all nodes are 
  * being asked to clean themselves up.
  * The cleanup phase is needed for event streams to release intermediary
  * results of calculations for garbage collection
  */
private[control] trait Node {

  /** True if this node has to be updated */
  def isDirty: Boolean

  /** True if this node changed during the update cycle */
  def hasChanged: Boolean

  /** Marks this node and all child nodes as dirty */
  def setDirty(): Unit

  /** Recalculates this node */
  def calculate(t: Time): Unit

  /** Cleans up this node
    *
    * This method is called after all nodes have been updated
    * during a graph traversal. It is used to release
    * resources to be garbage collector. Later it might also be
    * used to rearrange the node graph (adding and removing nodes)
    */
  def cleanup(): Unit

  /** Adds a child `Node` to this `Node`
    *
    * This `Node` as automatically added as a parent to the 
    * child's list of parents
    */
  def connectChild(n: ChildNode) { n.addParent(this); addChild(n) }

  protected def addChild(n: ChildNode): Unit
}

private[control] final class RootNode extends Node {
  private[this] val children = new ListBuffer[ChildNode]

  def isDirty = false
  def hasChanged = true
  def setDirty() { children foreach { _.setDirty } }
  def calculate(t: Time) { children foreach { _.calculate(t) } }
  def cleanup() { children foreach { _.cleanup() } }
  def addChild(n: ChildNode) { children += n }

  /** Updates the whole dependency graph
    *
    * This includes the following three phases
    *
    * Phase 1: Mark all changed nodes (and their child nodes etc.)
    *          as 'dirty' meaning each of them has to be updated
    *
    * Phase 2: Recalculate all dirty nodes
    *          Children are only recalculated, if all their parents have
    *          already ben recalculated (that is, they are no longer
    *          marked as dirty) and if at least one parent is
    *          marked as having changed.
    *
    * Phase 3: Cleanup all child nodes to release resources for
    *          garbage collection and to set them back to unchanged.
    *          A child is only cleaned up, if it is marked as having
    *          changed.
    */
  def update(t: Time) { setDirty(); calculate(t); cleanup() }
}

private[control] abstract class ChildNode extends Node {
  
  private[this] var dirty = false
  private[this] var changed = false
  private[this] val children = new ListBuffer[ChildNode]
  private[this] val parents = new ListBuffer[Node]

  protected def sourceEvents: Boolean = true
  protected def doClean()
  protected def doCalc(t: Time): Boolean

  final def isDirty = dirty
  final def hasChanged = changed
  final def addParent(n: Node) { parents += n }
  final def addChild(n: ChildNode) { children += n }

  final def setDirty() {
    if((!dirty) && sourceEvents) {
      dirty = true
      children foreach { _.setDirty() }
    }
  }

  final def calculate(t: Time) {
    if (dirty &&
        (! parents.exists(_.isDirty)) &&
        parents.exists(_.hasChanged)) {

      dirty = false

      if (doCalc(t)) {
        changed = true
        children foreach { _.calculate(t) }
      }
    }
  }

  final def cleanup() {
    if (hasChanged) {
      changed = false
      doClean()
      children foreach { _.cleanup() }
    }
  }
}

case object Isolated extends Node {
  def calculate(t: Time) {}
  def cleanup() {}
  def setDirty() {}
  def isDirty = false
  def hasChanged = false
  override def connectChild(n: ChildNode) {}
  protected def addChild(n: ChildNode) {}
}

// vim: set ts=2 sw=2 et:
