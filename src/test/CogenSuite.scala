package io.github.martinhh

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Prop
import org.scalacheck.rng.Seed

trait CogenSuite extends munit.ScalaCheckSuite:

  protected given arbSeed: Arbitrary[Seed] = Arbitrary(arbitrary[Long].map(Seed.apply))

  protected def equalValues[T](
    expectedCogen: Cogen[T]
  )(using arbSeed: Arbitrary[Seed], arbT: Arbitrary[T], derivedCogen: Cogen[T]): Prop =
    Prop.forAll { (s: Seed, t: T) =>
      assertEquals(derivedCogen.perturb(s, t), expectedCogen.perturb(s, t))
    }
