// package irreg
// 
// import spire.algebra.Order
// import spire.math.Interval
//
// trait Symbols[A] {
//   def size: Int
//   def last: Int
//   def pos(a: A): Int
//   def symbol(i: Int): A
// 
//   def partial(a1: A, a2: A): SymbolRange = {
//     val i = pos(a1)
//     val j = pos(a2)
//     if (i > j) EmptyRange
//     else if (i == 0 && j == last) FullRange
//     else PartialRange(i, j)
//   }
// }
// 
// object Symbols {
//   implicit val byteSymbols: Symbols[Byte] =
//     new Symbols[Byte] {
//       def size: Int = 256
//       def last: Int = 255
//       def pos(a: Byte): Int = a.toInt
//       def symbol(i: Int): Byte = i.toByte
//     }
// 
//   implicit val charSymbols: Symbols[Char] =
//     new Symbols[Char] {
//       def size: Int = 65536
//       def last: Int = 65535
//       def pos(a: Char): Int = a.toInt
//       def symbol(i: Int): Char = i.toChar
//     }
// }
// 
// 
// case object EmptyRange extends SymbolRange
// 
// case object FullRange extends SymbolRange
// 
// case class PartialRange private[irreg] (i: Int, j: Int) extends SymbolRange
// 
// trait SymbolRange {
//   def isEmpty: Boolean =
//     this == EmptyRange
// 
//   def size[A](implicit ev: Symbols[A]): Int =
//     this match {
//       case EmptyRange => 0
//       case FullRange => ev.size
//       case PartialRange(i, j) => j - i + 1
//     }
// 
//   def contains[A](a: A)(implicit ev: Symbols[A]): Boolean =
//     this match {
//       case EmptyRange =>
//         false
//       case FullRange =>
//         true
//       case PartialRange(i, j) =>
//         val k = ev.pos(a)
//         i <= k && k <= j
//     }
// 
//   def invert[A](implicit ev: Symbols[A]): List[SymbolRange] =
//     this match {
//       case EmptyRange =>
//         FullRange :: Nil
//       case FullRange =>
//         Nil
//       case PartialRange(i, j) =>
//         if (i == 0) PartialRange(j + 1, ev.last) :: Nil
//         else if (j == ev.last) PartialRange(0, i - 1) :: Nil
//         else PartialRange(0, i - 1) :: PartialRange(j + 1, ev.last) :: Nil
//     }
// }
