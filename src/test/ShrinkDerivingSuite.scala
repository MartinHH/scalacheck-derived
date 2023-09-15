package io.github.martinhh

import io.github.martinhh.derived.arbitrary.given
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

  test("shrinks to empty Stream for EmptyTuple") {
    assertEquals(deriveShrink[EmptyTuple].shrink(EmptyTuple), Stream.empty)
  }

  property("deriveShrink supports recursive structures") {
    equalValues(RecursiveList.expectedShrink[Int])(
      using anyGivenArbitrary,
      deriveShrink[RecursiveList[Int]]
    ) &&
    equalValues(NestedSumsRecursiveList.expectedShrink[Int])(
      using anyGivenArbitrary,
      deriveShrink[NestedSumsRecursiveList[Int]]
    ) &&
    equalValues(MaybeMaybeList.expectedShrink[Int])(
      using anyGivenArbitrary,
      deriveShrink[MaybeMaybeList[Int]]
    )
  }

  import io.github.martinhh.derived.shrink.given

  property("shrinks to the same values as non-derived expected Shrink (for simple case class)") {
    equalValues(SimpleCaseClass.expectedShrink)
  }

  property("shrinks to the same values as non-derived expected Shrink (for simple ADT)") {
    equalValues(SimpleADT.expectedShrink)
  }

  property("shrinks to the same values as non-derived expected Shrink (for simple Enum)") {
    equalValues(ABC.expectedShrink)
  }

  property("given derivation does not take precedence over existing givens") {
    equalValues(HasGivenInstances.specialHasGivenInstancesShrink)
  }

  property("given derivation of child-instances does not take precedence over existing givens") {
    equalValues(HasMemberThatHasGivenInstances.expectedShrink)
  }

  property("supports recursive structures") {
    equalValues(RecursiveList.expectedShrink[Int])
  }

  property("supports recursive structures (across nested sealed traits)") {
    equalValues(NestedSumsRecursiveList.expectedShrink[Int])
  }

  property("supports recursive structures (across more complex nested structures)") {
    equalValues(MaybeMaybeList.expectedShrink[Int])
  }

  property("supports direct recursion") {
    equalValues(DirectRecursion.expectedShrink)
  }

  // seems there is no feasible way to get up to par with ArbitraryDeriving, so this is just a
  // guard against making things even worse
  test("supports case classes with up to 25 fields (if -Xmax-inlines=32)") {
    summon[Shrink[MaxShrinkableCaseClass]]
  }

  // seems there is no feasible way to get up to par with ArbitraryDeriving, so this is just a
  // guard against making things even worse
  test("supports enums with up to 23 members (if -Xmax-inlines=32)") {
    summon[Shrink[MaxShrinkableEnum]]
  }
