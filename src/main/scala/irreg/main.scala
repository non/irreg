package irreg

import spire.algebra.Order
import spire.implicits._
import spire.random.mutable.Generator

import irreg.implicits._
import irreg.std.all._
import irreg.Regex._

object Main {

  def test(name: String, expr: Expr[Char], cases: List[String]) {
    println(s"test $name: ${expr.show}")
    println(s"  e.g. ${expr.sample(Generator.rng).mkString}")
    cases.foreach { s =>
      val b = expr.matches(s.toCharArray)
      val bb = expr.smatches(s.toStream)
      assert(b == bb)
      println(s"  $b -> $s")
    }
  }

  def main(args: Array[String]) {
    // test(
    //   "1",
    //   (v('b') + v('c')) * v('a') * v('t').kplus * v('s').kq,
    //   List("duck", "cat", "bat", "ba", "cattttt", "baatttttttttt", "catts")
    // )
    // 
    // test(
    //   "2",
    //   (v('a').pow(3) * upTo(v('b'), 2) * repeat(v('c'), 1, 2)) + v('d').kstar,
    //   List("", "ddddddddd", "aaac", "aaabbcc", "aaabb", "aaabbbcc", "aaaaaaaaabbcc")
    // )
    // 
    // test(
    //   "3",
    //   oneOf('a', 'b').kplus,
    //   List("", "aabbabab", "aa", "aaabbbcc", "caaaaa", "ccacacaca", "bbbbbb", "abababab")
    // )
    // 
    // val d19 = oneOf("123456789": _*)
    // val d09 = oneOf("0123456789": _*)
    // val e3 = d19 * d09.kstar * allOf(" bottles of beer on the wall": _*)
    // println(sample(e3, Generator.rng).mkString)
    // 
    // val r = oneOf('b', 'c', 'r') * v('a').kplus * v('t')
    // println(stream(r).take(10).map(_.mkString).toList)
    // 
    // val e4 = (v('0') + v('1')).kstar
    // val e5 = (v('0') + v('1')).pow(4)
    // val e6 = (v('0').kstar * v('1').kstar)
    // println(stream(e4).take(16).map(_.take(4).mkString).toList)
    // println(stream(e5).take(16).map(_.take(4).mkString).toList)
    // println(stream(e6).take(16).map(_.take(4).mkString).toList)
    // 
    // // // testing possible subsets, possible equality, approx cardinality
    // // val x1 = oneOf('0', '1').kstar
    // // val x2 = v('0').kstar + v('1').kstar
    // // //println(possibleSuperset(x1, x2, confidence=1000))
    // // println(relsize_!(x1, x2).take(20).toList)
    // // println(relativeSize(x1, x2, confidence=1000))
    // 
    // // testing diagonalization and interleaving
    // // val nss = Stream(Stream(1),Stream(2),Stream(3),Stream(4)).map(_.map(_.toString))
    // // println(StreamUtil.diagonalize(nss, nss).map(_.mkString).toList)
    // // val as = Stream(1,2,3,4,5)
    // // println(StreamUtil.interleave(as, as.map(_*10)).toList)

    def timer[A](f: => A): (Double, A) = {
      val t0 = System.nanoTime
      val a = f
      val t = System.nanoTime - t0
      (t / 1000000.0, a)
    }

    def bench[A](f: => A): (Double, A) = {
      val warmups = (0 until 4).map(_ => f)
      val runs = (0 until 8).map(_ => timer(f))
      val times = runs.map(_._1.toDouble)
      val result = runs.head._2
      (times.qmean, result)
    }

    def xyz[A: Order](e: Expr[A]): Dfa[A] = e.minimize

    val e1 = (v('f') + v('b')) * v('o') * (v('o').kstar + v('b'))
    val e2 = ((v('f') + v('b')) * v('o') * v('b')) + ((v('f') + v('b')) * v('o').kplus)

    val az = ('a' to 'z').map(v).qsum
    val e3 = az.kplus * v('@') * az.kplus * v('.') * az.kplus

    val (t1, dfa1) = bench(xyz(e1))
    val (t2, dfa2) = bench(xyz(e2))
    val (t3, dfa3) = bench(xyz(e3))
    println("took %.3fms to build %s" format (t1, dfa1.draw))
    println("took %.3fms to build %s" format (t2, dfa2.draw))
    println("are they equal? %s" format dfa1 == dfa2)

    println("took %.3fms to build %s" format (t2, dfa3.draw))

    println(dfa1.accept("bob"))
    println(dfa1.accept("boooo"))
    println(dfa1.accept("bobb"))
  }
}
