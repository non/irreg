package irreg

import language.experimental.macros

import spire.implicits._
import spire.macrosk.Ops

object implicits {
  implicit class ShowOps[A](a: A)(implicit ev: Show[A]) {
    def show(): String = macro Ops.unop[String]
  }

  implicit class StarRigOps[A](a: A)(implicit ev: StarRig[A]) {
    def kstar(): A = macro Ops.unop[A]
    def kplus(): A = macro Ops.unop[A]
    def kq(): A = ev.one + a
  }
}
