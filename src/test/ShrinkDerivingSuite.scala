package io.github.martinhh

import io.github.martinhh.derived.scalacheck.given
import io.github.martinhh.derived.shrink.given
import io.github.martinhh.derived.shrink.deriveShrink

import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import org.scalacheck.Shrink

@annotation.nowarn("msg=Stream .* is deprecated")
class ShrinkDerivingSuite extends munit.ScalaCheckSuite:

  private def equalValues[T](
    expectedShrink: Shrink[T],
    take: Int = 100
  )(using arbT: Arbitrary[T], derivedShrink: Shrink[T]): Prop =
    Prop.forAll { (t: T) =>
      assertEquals(derivedShrink.shrink(t).take(take), expectedShrink.shrink(t).take(take))
    }

  property("shrinks to the same values as non-derived expected Shrink (for simple case class)") {
    equalValues(SimpleCaseClass.expectedShrink)
  }

  property("shrinks to the same values as non-derived expected Shrink (for simple ADT)") {
    equalValues(SimpleADT.expectedShrink)
  }

  property("shrinks to the same values as non-derived expected Shrink (for simple Enum)") {
    equalValues(ABC.expectedShrink)
  }

  property("derivation does not take precedence over existing givens") {
    equalValues(HasGivenInstances.specialHasGivenInstancesShrink)
  }

  property("derivation of child-instances does not take precedence over existing givens") {
    equalValues(HasMemberThatHasGivenInstances.expectedShrink)
  }

  test("shrinks to empty Stream for EmptyTuple") {
    assertEquals(deriveShrink[EmptyTuple].shrink(EmptyTuple), Stream.empty)
  }

  // (should support as least as many fields as ArbitraryDeriving)
  test("supports case classes with up to 27 fields (if -Xmax-inlines=32)") {
    summon[Shrink[MaxCaseClass]]
  }

  // (should support as least as many fields as ArbitraryDeriving)
  test("supports enums with up to 25 members (if -Xmax-inlines=32)") {
    summon[Shrink[MaxEnum]]
  }
