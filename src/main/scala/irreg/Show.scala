package irreg

/**
 * Show is a type class we'll use to control how types should display.
 */
trait Show[A] {
  def show(a: A): String
}

object Show {
  def apply[A](implicit ev: Show[A]) = ev

  def gen[A](f: A => String): Show[A] = new Show[A] {
    def show(a: A): String = f(a)
  }
}
