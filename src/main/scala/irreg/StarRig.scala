package irreg

import spire.algebra.Rig

/**
 * StarRig[A] is a Rig[A] that also has an asteration operator: kstar.
 * 
 * Laws:
 * 1. a.star = 1 + a * a.star = 1 + a.star * a
 */
trait StarRig[A] extends Rig[A] {
  // one of these must be overridden in any type class instance
  def kstar(a: A): A = plus(one, kplus(a))
  def kplus(a: A): A = times(a, kstar(a))
}

object StarRig {
  def apply[A](implicit ev: StarRig[A]) = ev
}
