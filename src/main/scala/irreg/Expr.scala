package irreg

import spire.algebra.Eq
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
case class Or[A](lhs: Expr[A], rhs: Expr[A]) extends Expr[A]
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
      case Or(x, y) => "(" + show(x) + "|" + show(y) + ")"
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
      case (e1, e2) => Or(e1, e2)
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

  def empty[A]: Expr[A] = Empty
  def nul[A]: Expr[A] = Nul
  def v[A](a: A): Expr[A] = Var(a)
  def allOf[A](as: A*): Expr[A] = as.map(v).qproduct
  def oneOf[A](as: A*): Expr[A] = as.map(v).qsum

  def upTo[A](expr: Expr[A], n: Int): Expr[A] =
    (0 to n).map(i => expr.pow(i)).qsum
  def repeat[A](expr: Expr[A], m: Int, n: Int): Expr[A] =
    expr.pow(m) * upTo(expr, n - m)
}
