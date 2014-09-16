package irreg

import spire.algebra.Eq
import spire.implicits._
import spire.algebra._
import spire.math._
import spire.random.mutable.Generator
import irreg.implicits._
import irreg.std.all._
import StreamUtil._

// arguably everything here should just be moved into Expr[A].

/**
 * Regex defines some factory constructors that make building and
 * working with `Expr[A]` instances a bit easier.
 */
object Regex {

  def parse[A](s: String): Expr[A] = sys.error("TODO")

  def empty[A]: Expr[A] = Empty
  def nul[A]: Expr[A] = Nul
  def v[A](a: A): Expr[A] = Var(a)
  def allOf[A](as: A*): Expr[A] = as.map(v).qproduct
  def oneOf[A](as: A*): Expr[A] = as.map(v).qsum
  def str[A](as: Seq[A]): Expr[A] = as.map(v).qproduct

  def upTo[A](expr: Expr[A], n: Int): Expr[A] =
    (0 to n).map(i => expr.pow(i)).qsum

  def repeat[A](expr: Expr[A], m: Int, n: Int): Expr[A] =
    expr.pow(m) * upTo(expr, n - m)

  def check_![A: Eq](parent: Expr[A], child: Expr[A])(implicit alphabet: Stream[A]): Stream[Boolean] =
    child.stream.map(s => parent.matches(s))

  def possibleSuperset[A: Eq](parent: Expr[A], child: Expr[A], confidence: Int = 100)(implicit alphabet: Stream[A]): Boolean =
    check_!(parent, child).take(confidence).forall(_ == true)
}
