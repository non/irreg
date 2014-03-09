package irreg

import scala.collection.mutable

class Namer {
  private[this] var i = -1
  def apply(): Int = { i += 1; i }
}

class NameCache[A] {
  private[this] val namer = new Namer()
  private[this] val cache = mutable.Map.empty[A, Int]
  def apply(a: A): Int = cache.getOrElseUpdate(a, namer.apply)
  def names: Map[A, Int] = cache.toMap
}
