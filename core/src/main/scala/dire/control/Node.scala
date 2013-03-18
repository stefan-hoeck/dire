package dire.control

import dire.Time
import scalaz._, Scalaz._, effect.{IO, IORef}
import scalaz.Tags.Disjunction
import scalaz.Tag.subst

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
private[control] sealed trait Node {

  /** True if this node has to be recalculated */
  def isDirty: IO[Boolean @@ Disjunction]

  /** True if this node changed during the update cycle */
  def hasChanged: IO[Boolean @@ Disjunction]

  /** Marks this node and all child nodes as dirty */
  def setDirty: IO[Unit]

  /** Recalculates this node */
  def calculate(t: Time): IO[Unit]

  /** Cleans up this node
    *
    * This method is called after all nodes have been updated
    * during a graph traversal. It is used to release
    * resources to be garbage collector. Later it might also be
    * used to rearrange the node graph (adding and removing nodes)
    */
  def cleanup: IO[Unit]

  def connectChild(n: ChildNode): IO[Unit] =
    n.parents.mod { this :: _ } >> addChild(n)

  protected def addChild(n: ChildNode): IO[Unit]
}

private[control] case class ChildNode(
    node: SourceNode,
    parents: IORef[List[Node]])
  extends Node {
    import Node._
  def isDirty = node.isDirty
  def hasChanged = node.hasChanged
  def setDirty = node.setDirty
  def cleanup = node.cleanup
  protected def addChild(n: ChildNode) = node addChild n
  
  def calculate(t: Time) = {
    def mustCalc: IO[Boolean] = for {
      hasDirtyParent   ← runNodes(_.isDirty, parents)
      hasChangedParent ← runNodes(_.hasChanged, parents)
    } yield (!hasDirtyParent && hasChangedParent)

    onTrue(mustCalc){ node calculate t }
  }
}

private[control] case class SourceNode(
    dirty: IORef[Boolean],
    changed: IORef[Boolean],
    children: IORef[List[Node]],
    calc: Time ⇒ IO[Boolean],
    clean: IO[Unit])
  extends Node {
    import Node._

  def isDirty: IO[Boolean @@ Disjunction] = subst(dirty.read)

  def hasChanged: IO[Boolean @@ Disjunction] = subst(changed.read)

  def setDirty: IO[Unit] = dirty.write(true) >>
                           runNodes(_.setDirty, children)

  def calculate(t: Time) = onTrue(isDirty) {
    dirty.write(false) >>
    (calc(t) >>= { changed write _ }) >>
    runNodes(_ calculate t, children)
  }

  def cleanup: IO[Unit] = onTrue(hasChanged) {
    changed.write(false) >>
    clean >>
    runNodes(_.cleanup, children)
  }

  protected[control] def addChild(c: ChildNode) =
    children.mod(c :: _).void
}

case object Isolated extends Node {
  def calculate(t: Time) = IO.ioUnit
  def cleanup = IO.ioUnit
  def setDirty = IO.ioUnit
  def isDirty = IO(Node.FalseD)
  def hasChanged = IO(Node.FalseD)
  def addChild(n: ChildNode) = IO.ioUnit
}

object Node {
  private[control] final val FalseD = Disjunction(false)
  private final val NoNodes = IO.newIORef[List[Node]](Nil)
  private final val FalseR = IO.newIORef(false)

  def sourceNode(calc: Time ⇒ IO[Boolean], clean: IO[Unit]): IO[SourceNode] =
    ^^(FalseR, FalseR, NoNodes)(SourceNode(_,_,_,calc, clean))

  def childNode(calc: Time ⇒ IO[Boolean], clean: IO[Unit]): IO[ChildNode] =
    ^(sourceNode(calc, clean), NoNodes)(ChildNode.apply)

  private[control] def onTrue[A:Monoid](b: IO[Boolean])(f: IO[A]): IO[A] =
    b ifM (ifTrue = f, ifFalse = ∅[IO[A]])

  private[control] def onFalse[A:Monoid](b: IO[Boolean])(f: IO[A]): IO[A] =
    b ifM (ifFalse = f, ifTrue = ∅[IO[A]])

  private[control] def runNodes[A:Monoid](f: Node ⇒ IO[A],
                                          ns: IORef[List[Node]]): IO[A] =
    ns.read >>= { _ foldMap f }
}

// vim: set ts=2 sw=2 et:
