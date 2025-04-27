package io.github.martinhh

import io.github.martinhh.ABC.A
import io.github.martinhh.derived.RecursionFallback
import org.scalacheck.Gen

class ConfiguredArbitraryDerivingSuite extends test.ArbitrarySuite:

  private val simpleExample =
    derived.arbitrary.configured.simple([a] => (gens: List[Gen[a]]) => gens.head)

  test("configured.simple via explicit call") {
    equalArbitraryValues[ABC](Gen.const(ABC.A))(using simpleExample.deriveArbitrary)
  }

  test("configured.simple via given import") {
    import simpleExample.given
    equalArbitraryValues[ABC](Gen.const(ABC.A))
  }

  private trait Conf[A]:
    def select(gens: List[Gen[A]]): Gen[A]

  private val lastConf: Conf[ABC] =
    new Conf:
      override def select(gens: List[Gen[ABC]]): Gen[ABC] = gens.last

  private val confExample =
    derived.arbitrary.configured.customConf[
      Conf
    ]([a] => (gens: List[Gen[a]], c: Option[Conf[a]]) => c.fold(gens.head)(_.select(gens)))

  test("configured.customConf via explicit call without conf in scope") {
    equalArbitraryValues[ABC](Gen.const(ABC.A))(using confExample.deriveArbitrary)
  }

  test("configured.customConf via explicit call with conf in scope") {
    given Conf[ABC] = lastConf
    equalArbitraryValues[ABC](Gen.const(ABC.C))(using confExample.deriveArbitrary)
  }

  test("configured.customConf via given import without conf in scope") {
    import confExample.given
    equalArbitraryValues[ABC](Gen.const(ABC.A))
  }

  test("configured.customConf via given import with conf in scope") {
    import confExample.given
    given Conf[ABC] = lastConf
    equalArbitraryValues[ABC](Gen.const(ABC.C))
  }

  private val cRecursionFallback: RecursionFallback[ABC] = RecursionFallback(ABC.C)

  private val recursionFallbackExample =
    derived.arbitrary.configured.recursionFallback(
      [a] =>
        (gens: List[Gen[a]], rf: Option[RecursionFallback[a]]) => rf.fold(gens.head)(_.fallbackGen)
    )

  test("configured.recursionFallback via explicit call without RecursionFallback in scope") {
    equalArbitraryValues[ABC](Gen.const(ABC.A))(using recursionFallbackExample.deriveArbitrary)
  }

  test("configured.recursionFallback via explicit call with RecursionFallback in scope") {
    given RecursionFallback[ABC] = cRecursionFallback

    equalArbitraryValues[ABC](Gen.const(ABC.C))(using recursionFallbackExample.deriveArbitrary)
  }

  test("configured.recursionFallback via given import without RecursionFallback in scope") {
    import recursionFallbackExample.given
    equalArbitraryValues[ABC](Gen.const(ABC.A))
  }

  test("configured.recursionFallback via given import with RecursionFallback in scope") {
    import recursionFallbackExample.given
    given RecursionFallback[ABC] = cRecursionFallback

    equalArbitraryValues[ABC](Gen.const(ABC.C))
  }
