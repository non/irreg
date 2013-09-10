package irreg

import spire.algebra._
import spire.implicits._
import spire.random.Generator
import irreg.implicits._
import irreg.std.all._

/**
 * Expr[A] implements an AST for regular expressions.
 * 
 * Basic regular consist of the following:
 *  1. the empty set (Nul)        -- a set with no strings
 *  2. the empty string (Empty)   -- set containing the empty string
 *  3. literal strings (Var(a))   -- set containing a
 *  4. concatenation (Then(a, b)) -- set of all xy, for x in a, y in b
 *  5. alternation (Or(a, b))     -- union set of a and b
 *  6. kleene star (Star(a))      -- set produced by 0+ concatenations from a
 * 
 * For example, (a|bc)* includes "", "a", "bc", "abcaaaabc" but not "bc".
 */
sealed trait Expr[+A]
case class Var[A](a: A) extends Expr[A]
case class Or[A](es: List[Expr[A]]) extends Expr[A]
case class Then[A](lhs: Expr[A], rhs: Expr[A]) extends Expr[A]
case class Star[A](lhs: Expr[A]) extends Expr[A]
case object Empty extends Expr[Nothing]
case object Nul extends Expr[Nothing]

object Expr {
  def apply[A](a: A): Expr[A] = Var(a)

  implicit def exprHasShow[A: Show] = new Show[Expr[A]] {
    def show(e: Expr[A]) = e match {
      case Empty => "ε"
      case Nul => "∅"
      case Var(a) => a.show
      case Star(x) => show(x) + "*"
      case Or(es) => es.map(show(_)).mkString("(", "|", ")")
      case Then(x, y) => show(x) + show(y)
    }
  }

  implicit def exprHasKleene[A] = new Kleene[Expr[A]] {
    def zero: Expr[A] = Nul
    def one: Expr[A] = Empty

    def plus(x: Expr[A], y: Expr[A]): Expr[A] = (x, y) match {
      case (Nul, e) => e
      case (e, Nul) => e
      case (Empty, Empty) => Empty
      case (Empty, Star(e)) => Star(e)
      case (Star(e), Empty) => Star(e)
      case (Or(es1), Or(es2)) => Or(es1 ::: es2)
      case (Or(es1), e2) => Or(es1 :+ e2)
      case (e1, Or(es2)) => Or(e1 :: es2)
      case (e1, e2) => Or(e1 :: e2 :: Nil)
    }

    def times(x: Expr[A], y: Expr[A]): Expr[A] = (x, y) match {
      case (Nul, _) => Nul
      case (_, Nul) => Nul
      case (Empty, e) => e
      case (e, Empty) => e
      case (e1, e2) => Then(e1, e2)
    }

    override def kstar(x: Expr[A]): Expr[A] = x match {
      case Nul => Empty
      case Empty => Empty
      case Star(e) => kstar(e)
      case e => Star(e)
    }
  }

  implicit def order[A: Order] = new Order[Expr[A]] {
    def listCompare(x: List[Expr[A]], y: List[Expr[A]]): Int =
      (x, y) match {
        case (Nil, Nil) => 0
        case (Nil, bs) => -1
        case (as, Nil) => 1
        case (a :: as, b :: bs) =>
          val i = compare(a, b)
          if (i == 0) listCompare(as, bs) else i
      }

    def compare(x: Expr[A], y: Expr[A]): Int = (x, y) match {
      case (a, b) if a == b => 0
      case (Nul, _) => -1
      case (_, Nul) => 1
      case (Empty, _) => -1
      case (_, Empty) => 1
      case (Var(a), Var(b)) => a compare b
      case (Var(_), _) => -1
      case (_, Var(_)) => 1
      case (Or(es1), Or(es2)) => listCompare(es1, es2)
      case (Or(_), _) => -1
      case (_, Or(_)) => 1
      case (Then(a, b), Then(c, d)) =>
        val i = compare(a, c)
        if (i == 0) compare(b, d) else i
      case (Then(_, _), _) => -1
      case (_, Then(_, _)) => 1
      case (Star(a), Star(b)) => compare(a, b)
    }
  }
}
