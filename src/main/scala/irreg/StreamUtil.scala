package irreg

object StreamUtil {
  type SS[A] = Stream[Stream[A]]

  def diagonalize[A](x: SS[A], y: SS[A]): SS[A] = {
    def loop(i: Int): SS[A] = {
      val xs = x.take(i + 1).toArray
      val ys = y.take(i + 1).toArray
      if (i > xs.length + ys.length - 2) {
        Stream.empty
      } else {
        val s = (0 to i).toStream.flatMap { j =>
          val k = i - j
          if (j >= xs.length || k >= ys.length)
            None
          else
            Some(xs(j) #::: ys(k))
        }
        s #::: loop(i + 1)
      }
    }
    loop(0)
  }

  def interleave[B](x: Stream[B], y: Stream[B]): Stream[B] =
    if (x.isEmpty) y else x.head #:: interleave(y, x.tail)

  def concat[B](xss: List[Stream[B]]): Stream[B] =
    xss.foldLeft(Stream.empty[B])((s, xs) => s #::: xs)

  def interleave[B](xs: List[Stream[B]]): Stream[B] = xs match {
    case Nil => Stream.empty
    case x :: Nil => x
    case x :: y :: Nil => interleave(x, y)
    case x :: xs =>
      if (x.isEmpty) interleave(xs)
      else x.head #:: interleave(xs :+ x.tail) // ugh
  }
}
