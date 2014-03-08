package irreg

import spire.algebra._
import spire.implicits._
import spire.random.mutable.Generator
import irreg.implicits._
import irreg.std.all._
import StreamUtil._

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

  implicit def exprHasEq[A: Order] = new Eq[Expr[A]] {
    def eqv(x: Expr[A], y: Expr[A]): Boolean =
      Compiled(x).minimized == Compiled(y).minimized
  }

  implicit class ExprOps[A](lhs: Expr[A]) {

   /**
    * Perform a traditional regular expression match.
    * 
    * The match is anchored, requiring the entire input string to be
    * matched by expr.
    */
   def matches(string: IndexedSeq[A])(implicit ev: Eq[A]): Boolean = {
     def fits(pos: Int, a: A) = pos < string.length && string(pos) === a
     def look(expr: Expr[A], pos: Int): Stream[Int] =
       expr match {
         case Nul => Stream.empty
         case Empty => Stream(pos)
         case Var(a) => if (fits(pos, a)) Stream(pos + 1) else Stream.empty
         case Or(es) => concat(es.map(e => look(e, pos)))
         case Then(lhs, rhs) => look(lhs, pos).flatMap(n => look(rhs, n))
         case e @ Star(lhs) => pos #:: look(lhs, pos).flatMap(n => look(e, n))
       }
     look(lhs, 0).exists(_ == string.length)
   }
  
   /**
    * Perform a traditional regular expression match.
    * 
    * The match is anchored, requiring the entire input string to be
    * matched by expr.
    */
   def matches(stream: Stream[A])(implicit ev: Eq[A]): Boolean = {
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
         case Or(es) =>
           concat(es.map(e => look(e, stream)))
         case Then(lhs, rhs) =>
           look(lhs, stream).flatMap(s => look(rhs, s))
         case e @ Star(lhs) =>
           stream #:: look(lhs, stream).flatMap(s => look(e, s))
       }
     look(lhs, stream).exists(_.isEmpty)
   }
  
   /**
    * Stream all possible matching values.
    */
   def stream()(implicit ev: Eq[A]): Stream[Stream[A]] = {
     def iter(expr: Expr[A]): Stream[Stream[A]] =
       expr match {
         case Nul => Stream.empty
         case Empty => Stream(Stream.empty)
         case Var(a) => Stream(Stream(a))
         case Or(es) => interleave(es.map(iter))
         case Then(lhs, rhs) => diagonalize(iter(lhs), iter(rhs))
         case e @ Star(lhs) => Stream.empty #:: diagonalize(iter(lhs), iter(e))
       }
  
     iter(lhs)
   }
  
   /**
    * Sample an arbitrary matching value. Any value that could be
    * matched by expr could possibly be returned, although some values
    * are more likely to be generated than others.
    */
   def sample(rng: Generator): Stream[A] = {
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
         case Or(es) => choose(chooseFromList(es))
         case Then(lhs, rhs) => choose(lhs) #::: choose(rhs)
         case e @ Star(lhs) => if (yes) done else choose(lhs) #::: choose(e)
       }
     choose(lhs)
   }
  }
}

case class Compiled[A: Order](expr: Expr[A]) {
  def nfa: Nfa[A] = {
    def nfa(expr: Expr[A], namer: Namer): Nfa[A] =
      expr match {
        case Nul =>
          Nfa.empty[A](namer(), namer())
          
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
