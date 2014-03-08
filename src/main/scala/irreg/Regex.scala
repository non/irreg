package irreg

import spire.algebra.Eq
import spire.implicits._
import spire.algebra._
import spire.math._
import spire.random.mutable.Generator
import irreg.implicits._
import irreg.std.all._
import StreamUtil._

/**
 * Regex uses Expr objects to do interesting things.
 */
object Regex {

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

  def check_![A: Eq](parent: Expr[A], child: Expr[A]): Stream[Boolean] =
    child.stream.map(s => parent.matches(s))

  def possibleSuperset[A: Eq](parent: Expr[A], child: Expr[A], confidence: Int = 100): Boolean =
    check_!(parent, child).take(confidence).forall(_ == true)

  // def possiblyEqual[A: Eq](x: Expr[A], y: Expr[A], confidence: Int = 100): Boolean =
  //   interleave(check_!(x, y), check_!(y, x)).take(confidence).forall(_ == true)

  // // this is kind of cute. it could be a lot smarter.
  // def relsize_![A: Eq](x: Expr[A], y: Expr[A]): Stream[(Long, Long)] = {
  //   def xyz(rs: Stream[(Long, Long)], sn: Long, sd: Long): Stream[(Long, Long)] =
  //     rs match {
  //       case (n, d) #:: tail =>
  //         val tn = sn + n
  //         val td = sd + d
  //         (tn, td) #:: xyz(tail, tn, td)
  //       case _ =>
  //         Stream.empty
  //     }
  // 
  //   val xs = check_!(x, y).map(b => (if (b) 1L else 0L, 0L))
  //   val ys = check_!(y, x).map(b => (0L, if (b) 1L else 0L))
  // 
  //   xyz(interleave(xs, ys), 0L, 0L)
  // }
  // 
  // def relativeSize[A: Eq](x: Expr[A], y: Expr[A], confidence: Int = 100): Option[Rational] =
  //   relsize_!(x, y).
  //     take(confidence).
  //     lastOption.flatMap { case (x, y) =>
  //       if (x == 0 && y == 0) None else Some(Rational(x, x + y))
  //     }

  // // this currently only reorders ors. it should be doing much more
  // def canonical[A: Order](expr: Expr[A]): Expr[A] =
  //   expr match {
  //     case Or(es) => Or(es.qsorted)
  //     case Then(lhs, rhs) => Then(canonical(lhs), canonical(rhs))
  //     case Star(lhs) => Star(canonical(lhs))
  //     case e => e
  //   }
}
