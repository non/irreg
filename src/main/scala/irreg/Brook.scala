package irreg

import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.collection.immutable.{IndexedSeq, LinearSeq}
import scala.reflect.ClassTag

object &:: {
  def unapply[A](as: Brook[A]): Option[(A, Brook[A])] =
    if (as.isEmpty) None else Some((as.head, as.tail))
}

object Brook {
  def apply[A](as: A*): Brook[A] =
    as match {
      case as: IndexedSeq[A] => fromIndexedSeq(as)
      case as: LinearSeq[A] => fromLinearSeq(as)
      case as => as.foldRight(Brook.empty[A])((a, s) => Cons(a, s))
    }

  def fromIndexedSeq[A](as: IndexedSeq[A]): Brook[A] =
    range(0, as.length).map(as.apply)

  def fromLinearSeq[A](as: LinearSeq[A]): Brook[A] =
    if (as.isEmpty) End else Cons(as.head, fromLinearSeq(as.tail))

  def empty[A]: Brook[A] = End

  def unfold[A, B](b: B)(f: B => Option[(A, B)]): Brook[A] =
    f(b) match {
      case Some((a, b)) => Cons(a, unfold(b)(f))
      case None => End
    }

  def range(start: Int, limit: Int): Brook[Int] =
    if (start < limit) Cons(start, range(start + 1, limit))
    else empty

  def interleave[A](x: Brook[A], y: Brook[A]): Brook[A] =
    if (x.isEmpty) y else Cons(x.head, interleave(y, x.tail))

  def diagonalize[A](x: Brook[Brook[A]], y: Brook[Brook[A]]): Brook[Brook[A]] = {
    def loop(i: Int): Brook[Brook[A]] = {
      val xs = x.take(i + 1).toArray
      val ys = y.take(i + 1).toArray
      if (i > xs.length + ys.length - 2) {
        Brook.empty
      } else {
        val s = Brook.range(0, i).flatMap { j =>
          val k = i - j
          if (j >= xs.length || k >= ys.length)
            empty
          else
            Cons(xs(j) append ys(k))
        }
        s append loop(i + 1)
      }
    }
    loop(0)
  }

  implicit class Ops[A](x: Brook[A]) {
    def interleave(y: Brook[A]): Brook[A] =
      Brook.interleave(x, y)

    def toArray(implicit ct: ClassTag[A]): Array[A] = {
      val b = ArrayBuffer.empty[A]
      var as: Brook[A] = x
      while (!as.isEmpty) { b += as.head; as = as.tail }
      b.toArray
    }
  }

  implicit class PrependOps[A](rhs: => Brook[A]) {
    def &::(lhs: A): Brook[A] = Cons(lhs, rhs)
    def &:::(lhs: Brook[A]): Brook[A] = lhs append rhs
  }

  implicit class NestedOps[A](x: Brook[Brook[A]]) {
    def diagonalize(y: Brook[Brook[A]]): Brook[Brook[A]] =
      Brook.diagonalize(x, y)
  }
}

sealed trait Brook[+A] {
  def head: A
  def tail: Brook[A]

  def isEmpty: Boolean

  def append[B >: A](rest: => Brook[B]): Brook[B]

  override def toString: String =
    if (isEmpty) "Brook()" else s"Brook($head, ...)"

  def foldLeft[B](init: B)(f: (B, A) => B): B = {
    @tailrec def loop(b: B, as: Brook[A]): B =
      if (as.isEmpty) b else loop(f(b, as.head), as.tail)
    loop(init, this)
  }

  def length: Int =
    foldLeft(0)((n, _) => n + 1)

  def toList: List[A] = {
    val b = ListBuffer.empty[A]
    var as: Brook[A] = this
    while (!as.isEmpty) { b += as.head; as = as.tail }
    b.toList
  }

  def map[B](f: A => B): Brook[B] =
    if (isEmpty) End else Cons(f(head), tail.map(f))

  def flatMap[B](f: A => Brook[B]): Brook[B] =
    if (isEmpty) End else f(head) append tail.flatMap(f)

  def filter(f: A => Boolean): Brook[A] =
    if (isEmpty) End
    else if (f(head)) Cons(head, tail.filter(f))
    else tail.filter(f)

  def drop(n: Int): Brook[A] =
    if (n <= 0 || isEmpty) End else tail.drop(n - 1)

  def take(n: Int): Brook[A] =
    if (n <= 0 || isEmpty) End else Cons(head, tail.take(n - 1))

  def exists(f: A => Boolean): Boolean =
    if (isEmpty) false else f(head) || tail.exists(f)

  def forall(f: A => Boolean): Boolean =
    if (isEmpty) true else f(head) && tail.exists(f)
}

class Cons[A](val head: A, t: => Brook[A]) extends Brook[A] {
  def tail: Brook[A] = t
  def isEmpty: Boolean = false

  def append[B >: A](rest: => Brook[B]): Brook[B] = Cons(head, tail append rest)
}

object Cons {
  def apply[A](a: A) = new Cons(a, End)
  def apply[A](a: A, tail: => Brook[A]) = new Cons(a, tail)
}

object End extends Brook[Nothing] {
  def head: Nothing = sys.error("!")
  def tail: Nothing = sys.error("!")
  def isEmpty: Boolean = true

  def append[B >: Nothing](rest: => Brook[B]): Brook[B] = rest
}
