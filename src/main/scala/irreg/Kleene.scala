package irreg

/**
 * A Kleene is a StarRig which obeys some additional laws.
 *
 * Laws:
 * 1. a + a = a
 * 2. a * x + x = x  ==> a.kstar * x + x = x
 * 3. x * a + x = x  ==>  x * a.kstar + x = x
 */
trait Kleene[A] extends StarRig[A]

object Kleene {
  def apply[A](implicit ev: Kleene[A]) = ev
}
