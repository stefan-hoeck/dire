package dire.control

import collection.mutable.{ListBuffer ⇒ MList, HashMap ⇒ MMap}
import scala.reflect.runtime.universe._
import scalaz.effect.IO

/** A simple cache using `TypeTag`s for type safety **/
private[control] class Cache {
  private[this] val cache = new MMap[Any,MList[(Type,Any)]]

  def clear() = cache.clear()

  def apply[A:TypeTag](io: IO[A], key: Any): IO[A] = {
    val list = cache.getOrElseUpdate(key, new MList)
    val aType = typeOf[A]
    val found = list find { case (t,_) ⇒ t <:< aType }

    def create = for {
      a ← io
      _ ← IO(list += ((aType,a)))
    } yield a

    found.fold(create)(a ⇒ IO(a._2.asInstanceOf[A]))
  }
}

// vim: set ts=2 sw=2 et:
