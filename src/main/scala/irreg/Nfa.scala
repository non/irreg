package irreg

import scala.collection.mutable

object Nfa {
  def empty[A](start: Int, accept: Int) = new Nfa[A](start, accept, Map.empty)
}

case class Nfa[A](start: Int, accept: Int, edges: Map[Int, Map[Option[A], Set[Int]]]) {

  def accept(as: Seq[A]): Boolean = {
    def loop(as: List[A], states: Set[Int]): Boolean =
      as match {
        case Nil =>
          states(accept)
        case a :: tail =>
          val next = states.flatMap { n =>
            edges.get(n).flatMap(_.get(Some(a))).getOrElse(Set.empty[Int])
          }
          if (next.isEmpty) false else loop(tail, next)
      }
    loop(as.toList, Set(start))
  }

  def eps(from: Int, to: Int): Nfa[A] = add(from, None, to)

  def add(from: Int, c: Option[A], to: Int): Nfa[A] = {
    val m = edges.getOrElse(from, Map.empty)
    val set = m.getOrElse(c, Set.empty)
    Nfa(start, accept, edges.updated(from, m.updated(c, set + to)))
  }

  def follow(from: Int, c: Option[A]): Set[Int] =
    edges.getOrElse(from, Map.empty).getOrElse(c, Set.empty)

  // TODO could be more efficient
  def closure(from: Set[Int]): Set[Int] = {
    def iter(s: Set[Int]): Set[Int] = {
      val s2 = s | s.flatMap(n => follow(n, None))
      if (s2 == s) s else iter(s2)
    }
    iter(from)
  }

  def absorb(nfa: Nfa[A]): Nfa[A] = {
    val triples = nfa.edges.flatMap { case (from, m) =>
      m.flatMap { case (c, set) =>
        set.iterator.map(to => (from, c, to))
      }
    }
    triples.foldLeft(this) { case (nfa, (from, c, to)) => nfa.add(from, c, to) }
  }

  type P = Set[Int]

  def dfa()(implicit o: Ordering[A]): Dfa[A] = {
    val alphabet: Seq[A] = edges.values.flatMap {
      _ flatMap {
        case (Some(a), _) => Some(a)
        case _ => None
      }
    }.toSeq.sorted

    def loop(queue: List[P], sofar: Map[P, Map[A, P]]): Map[P, Map[A, P]] = {
      queue match {
        case Nil =>
          sofar
        case p :: ps =>
          if (sofar.contains(p)) {
            loop(ps, sofar)
          } else {
            val dests: Map[A, P] = alphabet.map { a =>
              (a, p.flatMap(n => closure(follow(n, Some(a)))))
            }.toMap
            loop(dests.values.toList ::: ps, sofar.updated(p, dests))
          }
      }
    }

    val st2: P = closure(Set(start))
    val m2: Map[P, Map[A, P]] = loop(st2 :: Nil, Map.empty)

    val cache = new NameCache[P]

    val start2: Int = cache(st2)

    val edges2: Map[Int, Map[A, Int]] =
      m2.flatMap { case (p, mm) =>
        val mm2 = for { (a, p) <- mm if p.nonEmpty } yield (a, cache(p))
        if (mm2.isEmpty) None else Some((cache(p), mm2))
      }

    val accept2: Set[Int] =
      (for { (p, n) <- cache.names if p(accept) } yield n).toSet

    Dfa(start2, accept2, edges2)
  }
}
