package org.scalacheck.io.github.martinhh

import org.scalacheck.Gen

@deprecated("This is not meant to be used as public api")
def failOnStackOverflow[A](gen: Gen[A]): Gen[A] =
  Gen.gen { (p, seed) =>
    try {
      gen.doApply(p, seed)
    } catch {
      case _: StackOverflowError =>
        Gen.failed[A](seed)
    }
  }
