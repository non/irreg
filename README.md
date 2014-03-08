## Irreg

### Overview

Irreg is a proof-of-concept regular expression library based on Kleene
algebras. It integrates with Spire's algebraic type classes to provide
regular expression capabilities for any type `A` that has a `Kleene[A]`
instance.

### Example

This is a work-in-progress. For now there are a ton of imports and the syntax
is a bit bulky. It is what it is.

```scala
import spire.implicits._
import spire.random.mutable.Generator

import irreg.implicits._
import irreg.std.all._
import irreg.Regex.{oneOf, allOf}

// mammals
val e = oneOf('c', 'b', 'r') * allOf('a', 't') // [cbr]a+t
e.matches("cat") // true
e.matches("bat") // true
e.matches("hat") // false

e.stream.map(_.mkString).toList // List(cat, bat, hat)

// lyric to a song
val d19 = oneOf("123456789": _*)
val d09 = oneOf("0123456789": _*)
val e3 = d19 * d09.kstar * allOf(" bottles of beer on the wall": _*)
sample(e3) // 87 bottles of beer on the wall
```
Once you've built an `Expr` instance, you can use the following methods:

 * `matches[A](s: IndexedSeq[A]): Boolean`: Determine
  whether `expr` accepts `s`. The entire input must be consumed.

 * `sample[A](rng: Generator): A`:
  Produces a random string that is accepted by `expr`.

 * `stream[A]: Stream[Stream[A]]`: Produce a
  (potentially-infinite) stream of all values accepted by `expr`.  A
  stream of streams is used to simulate laziness. You will probably
  want to `map` the inner streams into something more useful, and will
  also probably want to use `take` to avoid attempting to evaluate an
  infinite stream.

### Issues

 * Representing large character classes (e.g. "[a-z]") as pure
   alternations is a disaster for performance, and especially for
   NFA/DFA construction. This only gets worse when you consider all of
   Unicode. It would be great to add some kind of range element.

 * Having some kind of `LazyList` or `LazyString` might be a bit nicer
   than using `Stream` in some of these cases (e.g. we could avoid
   memoization cost in some cases, and we could also add some usefully
   lazy methods).

 * The current "DSL" for creating `Expr[A]` instances is quite ugly.
 
 * This project is going for generality first, performance later. I
   believe that it should be possible to let people work with the
   composable, flexible `Expr[A]` instances, and then eventually
   compile them into blazing fast `Compiled[A]` instances.

 * Since we are able to compile to a DFA we should be using that for
   matching, since it will be much faster.

 * Obviously since we are using "real" regular expressions there
   aren't any look-behind or look-ahead assertions. Also, we aren't
   doing anything special with identifying or matching subgroups.
   This bears looking into.
