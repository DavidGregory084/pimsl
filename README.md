# pimsl
**pimsl** is a small utility library for Scala.

[![Build Status](https://travis-ci.org/DavidGregory084/pimsl.svg?branch=master)](https://travis-ci.org/DavidGregory084/pimsl)

## Usage

### Arrows
**pimsl** offers a limited implementation of [John Hughes' Arrows](http://www.cse.chalmers.se/~rjmh/Papers/arrows.pdf) in Scala, including compatibility with sequence comprehensions in a way that imitates Haskell's `proc` notation:

```scala
scala> import pimsl.Arrow._
import pimsl.Arrow._

scala> val addOne: Int => Int = a => a + 1
addOne: Int => Int = <function1>

scala> val addTwo: Int => Int = a => a + 2
addOne: Int => Int = <function1>

scala> :paste
// Entering paste mode (ctrl-D to finish)

val proc: Int => Int = x => for {
  y <- addOne -< x
  z <- addTwo -< x
} yield y + z

// Exiting paste mode, now interpreting.

proc: Int => Int = <function1>

scala> proc(0)
res0: Int = 3
```

**Author**

[David Gregory](https://github.com/DavidGregory084)
