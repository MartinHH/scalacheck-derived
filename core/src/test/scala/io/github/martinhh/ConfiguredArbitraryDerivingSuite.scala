package io.github.martinhh

import io.github.martinhh.ABC.A
import io.github.martinhh.derived.ArbitraryDeriving
import org.scalacheck.Gen

class ConfiguredArbitraryDerivingSuite extends test.ArbitrarySuite:

  private trait Conf[A]:
    def select(gens: List[Gen[A]]): Gen[A]

  private val lastConf: Conf[ABC] =
    new Conf:
      override def select(gens: List[Gen[ABC]]): Gen[ABC] = gens.last

  private object confExample extends ArbitraryDeriving[Conf]:
    def buildSumGen[A](gens: List[Gen[A]], config: Option[Conf[A]]): Gen[A] =
      config.fold(gens.head)(_.select(gens))

  test("custom conf via explicit call without conf in scope") {
    equalArbitraryValues[Maybe[ABC]](Gen.const(Maybe.Defined(ABC.A)))(
      using confExample.deriveArbitrary
    )
  }

  test("custom conf via explicit call with conf in scope") {
    given Conf[ABC] = lastConf
    equalArbitraryValues[Maybe[ABC]](Gen.const(Maybe.Defined(ABC.C)))(
      using confExample.deriveArbitrary
    )
  }

  test("custom conf via given import without conf in scope") {
    import confExample.given
    equalArbitraryValues[Maybe[ABC]](Gen.const(Maybe.Defined(ABC.A)))
  }

  test("custom conf via given import with conf in scope") {
    import confExample.given
    given Conf[ABC] = lastConf
    equalArbitraryValues[Maybe[ABC]](Gen.const(Maybe.Defined(ABC.C)))
  }
