## Irreg

### Overview

Irreg is a proof-of-concept regular expression library based on Kleene
algebras. It integrates with Spire's algebraic type classes to provide regular
expression capabilities for any type that has a Kleene[_] instance.

### Example

This is a work-in-progress. For now there are a ton of imports and the syntax
is a bit bulky. It is what it is.

```scala
import spire.implicits._
import spire.random.Generator

import irreg.implicits._
import irreg.std.all._
import irreg.Regex._

// mammals
val e = oneOf('c', 'b', 'r') * allOf('a', 't') // [cbr]a+t
matches(e, "cat") // true
matches(e, "bat") // true
matches(e, "hat") // false

stream(e).map(_.mkString).take(10).toList
// List(bat, baat, rat, baaat, raat, cat, baaaat, raaat, caat, baaaaat)

// lyric to a song
val d19 = oneOf("123456789": _*)
val d09 = oneOf("0123456789": _*)
val e3 = d19 * d09.kstar * allOf(" bottles of beer on the wall": _*)
sample(e3) // 87 bottles of beer on the wall
```

The three interesint methods in `Regex` are:

 * `matches[A](expr: Expr[A], s: IndexedSeq[A]): Boolean`:
  Determine whether `s` matches `expr`. The entire input must be consumed.

 * `sample[A](expr: Expr[A], rng: Generator): A`:
  Produces a random string that is accepted by `expr`.

 * `stream[A](expr: Expr[A]): Stream[Stream[A]]`:
  Produce a (potentially-infinite) stream of all values accepted by `expr`.
  A stream of streams is used to simulate laziness. You will probably want
  to `map` the inner streams into something more useful, and will also
  probably want to use `take` to avoid evaluating the entire stream.

### Issues

 * In the interest of staying close to the underlying boolean algebra, we
   don't currently have an `Expr[A]` subtype to represent `oneOf`
   directly. This affects the likelihood of seeing certain words from
   `sample` as well as the order words appear in `stream`. It probably
   make sense to fix this.

 * The current "DSL" for creating `Expr[A]` instances is ugly.

 * I haven't measured performance, but I'm not holding my breath. This
   project is going for generality first, performance later.

 * Obviously since we are using "real" regular expressions there aren't
   any look-behind or look-ahead assertions. Also, we aren't doing
   anything special with identifying or matching subgroups.
