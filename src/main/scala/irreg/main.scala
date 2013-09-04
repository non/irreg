package irreg

import spire.implicits._
import spire.random.Generator

import irreg.implicits._
import irreg.std.all._
import irreg.Expr.{empty, nul, v, allOf, oneOf, upTo, repeat}

object Main {

  def test(name: String, expr: Expr[Char], cases: List[String]) {
    println(s"test $name: ${expr.show}")
    println(s"  e.g. ${Regex.sample(expr, Generator.rng).mkString}")
    cases.foreach { s =>
      val b = Regex.matches(expr, s.toCharArray)
      println(s"  ${b.show} -> $s")
    }
  }

  def main(args: Array[String]) {
    test(
      "1",
      (v('b') + v('c')) * v('a') * v('t').kplus * v('s').kq,
      List("duck", "cat", "bat", "ba", "cattttt", "baatttttttttt", "catts")
    )

    test(
      "2",
      (v('a').pow(3) * upTo(v('b'), 2) * repeat(v('c'), 1, 2)) + v('d').kstar,
      List("", "ddddddddd", "aaac", "aaabbcc", "aaabb", "aaabbbcc", "aaaaaaaaabbcc")
    )

    test(
      "3",
      oneOf('a', 'b').kplus,
      List("", "aabbabab", "aa", "aaabbbcc", "caaaaa", "ccacacaca", "bbbbbb", "abababab")
    )
  }
}
