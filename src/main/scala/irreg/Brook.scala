package irreg

import scala.annotation.tailrec
import scala.collection.mutable

object #:: {
  def unapply[A](as: Brook[A]): Option[(A, Brook[A])] =
    if (as.isEmpty) None else Some((as.head, as.tail))
}

object Brook {
  def apply[A](a: A): Brook[A] = new Cons(a, End)
  def empty[A]: Brook[A] = End

  implicit class Prepend[A](rhs: => Brook[A]) {
    def #::(lhs: A): Brook[A] = new Cons(lhs, rhs)
    def #:::(lhs: Brook[A]): Brook[A] = lhs append rhs
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
    val b = mutable.ListBuffer.empty[A]
    var as: Brook[A] = this
    while (!as.isEmpty) { b += as.head; as = as.tail }
    b.toList
  }

  def map[B](f: A => B): Brook[B] =
    if (isEmpty) End else new Cons(f(head), tail.map(f))

  def flatMap[B](f: A => Brook[B]): Brook[B] =
    if (isEmpty) End else f(head) append tail.flatMap(f)

  def filter(f: A => Boolean): Brook[A] =
    if (isEmpty) End
    else if (f(head)) new Cons(head, tail.filter(f))
    else tail.filter(f)

  def drop(n: Int): Brook[A] =
    if (n <= 0 || isEmpty) End else tail.drop(n - 1)

  def take(n: Int): Brook[A] =
    if (n <= 0 || isEmpty) End else new Cons(head, tail.take(n - 1))

  def exists(f: A => Boolean): Boolean =
    if (isEmpty) false else f(head) || tail.exists(f)

  def forall(f: A => Boolean): Boolean =
    if (isEmpty) true else f(head) && tail.exists(f)
}

class Cons[A](val head: A, t: => Brook[A]) extends Brook[A] {
  def tail: Brook[A] = t
  def isEmpty: Boolean = false

  def append[B >: A](rest: => Brook[B]): Brook[B] = new Cons(head, tail append rest)
}

object End extends Brook[Nothing] {
  def head: Nothing = sys.error("!")
  def tail: Nothing = sys.error("!")
  def isEmpty: Boolean = true

  def append[B >: Nothing](rest: => Brook[B]): Brook[B] = rest
}
