package io.github.martinhh

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Gen.Parameters
import org.scalacheck.rng.Seed

class ArbitrarySuite extends munit.FunSuite:

  protected def equalValues[T](
    expectedGen: Gen[T],
    nTests: Int = 100
  )(using arbUnderTest: Arbitrary[T]): Unit =
    (0 until nTests).foldLeft(Seed.random()) { case (seed, _) =>
      val expected = expectedGen(Parameters.default, seed)
      val derived = arbUnderTest.arbitrary(Parameters.default, seed)
      assertEquals(derived, expected, s"Differing values for seed $seed")
      seed.next
    }
