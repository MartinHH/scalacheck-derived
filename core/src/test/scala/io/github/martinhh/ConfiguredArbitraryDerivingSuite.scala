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
