package irreg

import scala.collection.mutable

case class Dfa[A](start: Int, accept: Set[Int], edges: Map[Int, Map[A, Int]]) {

  def accept(as: Seq[A]): Boolean = {
    def loop(as: List[A], state: Int): Boolean =
      as match {
        case Nil =>
          accept(state)
        case a :: tail =>
          edges.get(state).flatMap(m => m.get(a)) match {
            case Some(n) => loop(tail, n)
            case None => false
          }
      }
    loop(as.toList, start)
  }

  def draw(implicit o: Ordering[A]): String =
    edges.map { case (p, m) =>
      (p, m.map { case (k, v) =>
        if (accept(v)) s"$k→[$v]" else s"$k→$v"
      }.mkString("{", ", ", "}"))
    }.toSeq.sorted.map {
      case (`start`, v) => s"($start):$v"
      case (k, v) => s"$k:$v"
    }.mkString(" ")

  def states: Set[Int] =
    Set(start) | accept | edges.keys.toSet | edges.values.flatMap(_.values).toSet

  def minimize()(implicit o: Ordering[A]): Dfa[A] = {
    type P = Set[Int]
    val universe = states.toSeq.sorted
    val p = mutable.Set(accept, universe.toSet -- accept)
    val w = mutable.Set(accept)

    val alphabet: Seq[A] = edges.values.flatMap(_.keys).toSeq.sorted

    while (!w.isEmpty) {
      val a = w.head
      w.remove(a)
      alphabet.foreach { c =>
        val x: P = edges.flatMap {
          case (from, transitions) if transitions.get(c).filter(a).isDefined => Some(from)
          case _ => None
        }.toSet
        p.foreach { y =>
          val intersect = y & x
          if (!intersect.isEmpty) {
            p.remove(y)
            p.add(intersect)
            val diff = y -- x
            p.add(diff)
            if (w(y)) {
              w.remove(y)
              w.add(intersect)
              w.add(diff)
            } else if (intersect.size <= diff.size) {
              w.add(intersect)
            } else {
              w.add(diff)
            }
          }
        }
      }
    }

    val rewrite: Map[Int, Int] = p.filter(!_.isEmpty).flatMap { s =>
      val h = s.min
      s.map(n => (n, h))
    }.toMap

    def isPrimary(n: Int): Boolean = rewrite.getOrElse(n, -1) == n

    def loop(queue: List[Int], idx: Int, sofar: Map[Int, Int]): Map[Int, Int] =
      queue match {
        case Nil =>
          sofar
        case n :: ns if isPrimary(n) && !sofar.contains(n) =>
          val sofar2 = sofar.updated(n, idx)
          edges.get(n) match {
            case Some(m) =>
              loop(ns ++ alphabet.flatMap(m.get), idx + 1, sofar2)
            case None =>
              loop(ns, idx + 1, sofar2)
          }
        case _ :: ns =>
          loop(ns, idx, sofar)
      }

    val normalized = loop(start :: Nil, 0, Map.empty)

    val start2 = normalized(rewrite(start))
    val accept2 = accept.map(x => normalized(rewrite(x)))
    val edges2 = universe.flatMap { from =>
      if (rewrite(from) == from)
        edges.get(from).map(ts => (normalized(rewrite(from)), ts.map { case (c, to) => (c, normalized(rewrite(to))) }))
      else
        None
    }.toMap

    Dfa(start2, accept2, edges2)
  }
}
