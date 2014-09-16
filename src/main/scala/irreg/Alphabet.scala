// package irreg
// 
// import spire.algebra._
// import spire.implicits._
// 
// import Predef.{any2stringadd => _, _}
// 
// trait Alphabet[A] {
//   def order: Order[A]
// 
//   def first: A
//   def last: A
//   def stream: Stream[A]
// }
// 
// class RigBasedAlphabet[A: Order: Rig](val first: A, val last: A) extends Alphabet[A] {
//   val order = Order[A]
//   def stream: Stream[A] = Alphabet.makeStream(first, last)
// }
// 
// class FixedAlphabet[A: Order](members: Seq[A]) extends Alphabet[A] {
//   val order = Order[A]
//   def stream: Stream[A] = members.toStream
// }
// 
// object Alphabet {
// 
//   def makeStream[A](first: A, last: A)(implicit e: Eq[A], r: Rig[A]): Stream[A] =
//     if (first === last) last #:: Stream.empty[A]
//     else first #:: makeStream[A](first + r.one, last)
// 
//   implicit val bytes: Alphabet[Byte] =
//     new RigBasedAlphabet(0.toByte, -1.toByte)
// 
//   implicit val shorts: Alphabet[Short] =
//     new RigBasedAlphabet(0.toShort, -1.toShort)
// 
//   implicit val chars: Alphabet[Char] =
//     new Alphabet[Char] {
//       val order = Order[Char]
//       def first: Char = 0.toChar
//       def last: Char = -1.toChar
//       def stream: Stream[Char] = (0 to 0xffff).toStream.map(_.toChar)
//     }
// 
//   implicit val ints: Alphabet[Int] =
//     new RigBasedAlphabet(0, -1)
// }
