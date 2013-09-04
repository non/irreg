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
val e1 = (v('c') + v('b') + v('r')) * v('a') * v('t') // [cbr]at
val e2 = oneOf('c', 'b', 'r') * allOf('a', 't') // equivalent
matches(e1, "cat") // true
matches(e1, "bat") // true
matches(e1, "hat") // false

// lyric to a song
val d19 = oneOf("123456789": _*)
val d09 = oneOf("0123456789": _*)
val e3 = d19 * d09.kstar * allOf(" bottles of beer on the wall": _*)
sample(e3) // 87 bottles of beer on the wall

```
