package io.github.martinhh.test

import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import org.scalacheck.Shrink

trait ShrinkSuite extends munit.ScalaCheckSuite:
  protected def equalShrinkValues[T](
    expectedShrink: Shrink[T],
    take: Int = 100
  )(using arbT: Arbitrary[T], derivedShrink: Shrink[T]): Prop =
    Prop.forAll { (t: T) =>
      assertEquals(derivedShrink.shrink(t).take(take), expectedShrink.shrink(t).take(take))
    }
