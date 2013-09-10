package irreg

import spire.algebra.Eq
import spire.implicits._
import spire.algebra._
import spire.math._
import spire.random.Generator
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
        //case Or(lhs, rhs) => look(lhs, pos) #::: look(rhs, pos)
        case Or(es) => concat(es.map(e => look(e, pos)))
        case Then(lhs, rhs) => look(lhs, pos).flatMap(n => look(rhs, n))
        case e @ Star(lhs) => pos #:: look(lhs, pos).flatMap(n => look(e, n))
      }
    look(expr, 0).exists(_ == string.length)
  }

  /**
   * Perform a traditional regular expression match.
   * 
   * The match is anchored, requiring the entire input string to be
   * matched by expr.
   */
  def smatches[A: Eq](expr: Expr[A], stream: Stream[A]): Boolean = {
    def look(expr: Expr[A], stream: Stream[A]): Stream[Stream[A]] =
      expr match {
        case Nul =>
          Stream.empty
        case Empty =>
          Stream(stream)
        case Var(a) =>
          stream match {
            case `a` #:: tail => Stream(tail)
            case _ => Stream.empty
          }
        // case Or(lhs, rhs) =>
        //   look(lhs, stream) #::: look(rhs, stream)
        case Or(es) =>
          concat(es.map(e => look(e, stream)))
        case Then(lhs, rhs) =>
          look(lhs, stream).flatMap(s => look(rhs, s))
        case e @ Star(lhs) =>
          stream #:: look(lhs, stream).flatMap(s => look(e, s))
      }
    look(expr, stream).exists(_.isEmpty)
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
        //case Or(lhs, rhs) => interleave(iter(lhs), iter(rhs))
        case Or(es) => interleave(es.map(iter))
        case Then(lhs, rhs) => diagonalize(iter(lhs), iter(rhs))
        case e @ Star(lhs) => Stream.empty #:: diagonalize(iter(lhs), iter(e))
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

    def chooseFromList(es: List[Expr[A]]): Expr[A] = {
      def loop(curr: Expr[A], n: Int, es: List[Expr[A]]): Expr[A] =
        es match {
          case e :: es => loop(if (rng.nextInt(n + 1) > 0) curr else e, n + 1, es)
          case Nil => curr
        }
      es match {
        case e :: es => loop(e, 1, es)
        case Nil => sys.error("!!!")
      }
    }

    def choose(expr: Expr[A]): Stream[A] =
      expr match {
        case Nul => done
        case Empty => done
        case Var(a) => Stream(a)
        //case Or(lhs, rhs) => choose(if (yes) lhs else rhs)
        case Or(es) => choose(chooseFromList(es))
        case Then(lhs, rhs) => choose(lhs) #::: choose(rhs)
        case e @ Star(lhs) => if (yes) done else choose(lhs) #::: choose(e)
      }
    choose(expr)
  }

  def check_![A: Eq](parent: Expr[A], child: Expr[A]): Stream[Boolean] =
    stream(child).map(s => smatches(parent, s))

  def possibleSuperset[A: Eq](parent: Expr[A], child: Expr[A], confidence: Int = 100): Boolean =
    check_!(parent, child).take(confidence).forall(_ == true)

  def possiblyEqual[A: Eq](x: Expr[A], y: Expr[A], confidence: Int = 100): Boolean =
    interleave(check_!(x, y), check_!(y, x)).take(confidence).forall(_ == true)

  // this is kind of cute. it could be a lot smarter.
  def relsize_![A: Eq](x: Expr[A], y: Expr[A]): Stream[(Long, Long)] = {
    def xyz(rs: Stream[(Long, Long)], sn: Long, sd: Long): Stream[(Long, Long)] =
      rs match {
        case (n, d) #:: tail =>
          val tn = sn + n
          val td = sd + d
          (tn, td) #:: xyz(tail, tn, td)
        case _ =>
          Stream.empty
      }

    val xs = check_!(x, y).map(b => (if (b) 1L else 0L, 0L))
    val ys = check_!(y, x).map(b => (0L, if (b) 1L else 0L))

    xyz(interleave(xs, ys), 0L, 0L)
  }

  def relativeSize[A: Eq](x: Expr[A], y: Expr[A], confidence: Int = 100): Option[Rational] =
    relsize_!(x, y).
      take(confidence).
      lastOption.flatMap { case (x, y) =>
        if (x == 0 && y == 0) None else Some(Rational(x, x + y))
      }

  // this currently only reorders ors. it should be doing much more
  def canonical[A: Order](expr: Expr[A]): Expr[A] =
    expr match {
      case Or(es) => Or(es.qsorted)
      case Then(lhs, rhs) => Then(canonical(lhs), canonical(rhs))
      case Star(lhs) => Star(canonical(lhs))
      case e => e
    }
}
