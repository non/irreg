package irreg

import spire.algebra.Eq
import spire.implicits._
import spire.random.Generator
import irreg.implicits._
import irreg.std.all._

/**
 * Regex uses Expr objects to do interesting things.
 */
object Regex {

  def empty[A]: Expr[A] = Empty
  def nul[A]: Expr[A] = Nul
  def v[A](a: A): Expr[A] = Var(a)
  def allOf[A](as: A*): Expr[A] = as.map(v).qproduct
  def oneOf[A](as: A*): Expr[A] = as.map(v).qsum

  def upTo[A](expr: Expr[A], n: Int): Expr[A] =
    (0 to n).map(i => expr.pow(i)).qsum
  def repeat[A](expr: Expr[A], m: Int, n: Int): Expr[A] =
    expr.pow(m) * upTo(expr, n - m)

  /**
   * Perform a traditional regular expression match.
   * 
   * The match is anchored, requiring the entire input string to be
   * matched by expr.
   */
  def matches[A: Eq](expr: Expr[A], string: IndexedSeq[A]): Boolean = {
    def fits(pos: Int, a: A) = pos < string.length && string(pos) === a
    def look(expr: Expr[A], pos: Int): Stream[Int] =
      expr match {
        case Nul => Stream.empty
        case Empty => Stream(pos)
        case Var(a) => if (fits(pos, a)) Stream(pos + 1) else Stream.empty
        case Or(lhs, rhs) => look(lhs, pos) #::: look(rhs, pos)
        case Then(lhs, rhs) => look(lhs, pos).flatMap(n => look(rhs, n))
        case e @ Star(lhs) => pos #:: look(lhs, pos).flatMap(n => look(e, n))
      }
    look(expr, 0).exists(_ == string.length)
  }

  /**
   * Stream all possible matching values.
   */
  def stream[A: Eq](expr: Expr[A]): Stream[Stream[A]] = {
    def iter(expr: Expr[A]): Stream[Stream[A]] =
      expr match {
        case Nul => Stream.empty
        case Empty => Stream(Stream.empty)
        case Var(a) => Stream(Stream(a))
        case Or(lhs, rhs) => iter(lhs) #::: iter(rhs)
        case Then(lhs, rhs) =>
          val rs = iter(rhs)
          iter(lhs).flatMap(s => rs.map(s #::: _))
        case e @ Star(lhs) =>
          Stream.empty[A] #:: iter(lhs).flatMap(s => iter(e).map(s #::: _))
      }
    iter(expr)
  }

  /**
   * Sample an arbitrary matching value. Any value that could be
   * matched by expr could possibly be returned, although some values
   * are more likely to be generated than others.
   */
  def sample[A](expr: Expr[A], rng: Generator): Stream[A] = {
    val done = Stream.empty[A]
    def yes = rng.nextBoolean
    def choose(expr: Expr[A]): Stream[A] =
      expr match {
        case Nul => done
        case Empty => done
        case Var(a) => Stream(a)
        case Or(lhs, rhs) => choose(if (yes) lhs else rhs)
        case Then(lhs, rhs) => choose(lhs) #::: choose(rhs)
        case e @ Star(lhs) => if (yes) done else choose(lhs) #::: choose(e)
      }
    choose(expr)
  }
}
