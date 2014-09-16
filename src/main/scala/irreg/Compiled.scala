package irreg

import spire.algebra._

case class Compiled[A: Order](expr: Expr[A])(implicit alphabet: List[A]) {
  def nfa: Nfa[A] = {
    def nfa(expr: Expr[A], namer: Namer): Nfa[A] =
      expr match {
        case Nul =>
          Nfa.empty[A](namer(), namer())

        case Dot =>
          nfa(Or(alphabet.map(Var(_))), namer)
          
        case Empty =>
          val start = namer()
          val accept = namer()
          Nfa.empty[A](start, accept).eps(start, accept)
          
        case Var(a) =>
          val start = namer()
          val accept = namer()
          Nfa.empty[A](start, accept).add(start, Some(a), accept)
          
        case Or(es) =>
          val start = namer()
          val nfas = es.map(e => nfa(e, namer))
          val accept = namer()
          nfas.foldLeft(Nfa.empty[A](start, accept)) { (nfa, e) =>
            nfa.absorb(e).eps(start, e.start).eps(e.accept, accept)
          }
          
        case Then(lhs, rhs) =>
          val e1 = nfa(lhs, namer)
          val e2 = nfa(rhs, namer)
          Nfa.empty[A](e1.start, e2.accept).
            absorb(e1).absorb(e2).eps(e1.accept, e2.start)
          
        case Star(lhs) =>
          val start = namer()
          val e1 = nfa(lhs, namer)
          val accept = namer()
          Nfa.empty[A](start, accept).absorb(e1).
            eps(start, e1.start).eps(e1.accept, accept).eps(accept, e1.start)
      }
    nfa(expr, new Namer())
  }
  
  def dfa: Dfa[A] = nfa.dfa
  
  def minimized: Dfa[A] = nfa.dfa.minimize
}

object Compiled {
  implicit def compiledHasEq[A] = new Eq[Compiled[A]] {
    def eqv(x: Compiled[A], y: Compiled[A]): Boolean =
      x.minimized == y.minimized
  }
}
