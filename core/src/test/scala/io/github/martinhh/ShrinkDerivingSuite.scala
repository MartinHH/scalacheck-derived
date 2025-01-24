package io.github.martinhh

import io.github.martinhh.derived.arbitrary.given
import io.github.martinhh.derived.shrink.deriveShrink

import org.scalacheck.Arbitrary
import org.scalacheck.Shrink

@annotation.nowarn("msg=Stream .* is deprecated")
class ShrinkDerivingSuite extends test.ShrinkSuite:

  test("shrinks to empty Stream for EmptyTuple") {
    assertEquals(deriveShrink[EmptyTuple].shrink(EmptyTuple), Stream.empty)
  }

  property("deriveShrink supports recursive structures") {
    equalShrinkValues(RecursiveList.expectedShrink[Int])(
      using anyGivenArbitrary,
      deriveShrink[RecursiveList[Int]]
    ) &&
    equalShrinkValues(NestedSumsRecursiveList.expectedShrink[Int])(
      using anyGivenArbitrary,
      deriveShrink[NestedSumsRecursiveList[Int]]
    ) &&
    equalShrinkValues(MaybeMaybeList.expectedShrink[Int])(
      using anyGivenArbitrary,
      deriveShrink[MaybeMaybeList[Int]]
    )
  }

  test("deriveShrink uses existing given factories (e.g. for Lists)") {
    equalShrinkValues(CaseClassWithListOfCaseClass.expectedShrink)(
      using anyGivenArbitrary,
      derived.shrink.deriveShrink
    )
  }

  import io.github.martinhh.derived.shrink.given

  property("shrinks to the same values as non-derived expected Shrink (for simple case class)") {
    equalShrinkValues(SimpleCaseClass.expectedShrink)
  }

  property("shrinks to the same values as non-derived expected Shrink (for simple ADT)") {
    equalShrinkValues(SimpleADT.expectedShrink)
  }

  property("shrinks to the same values as non-derived expected Shrink (for simple Enum)") {
    equalShrinkValues(ABC.expectedShrink)
  }

  property("given derivation does not take precedence over existing givens") {
    equalShrinkValues(HasGivenInstances.specialHasGivenInstancesShrink)
  }

  property("given derivation of child-instances does not take precedence over existing givens") {
    equalShrinkValues(HasMemberThatHasGivenInstances.expectedShrink)
  }

  test(
    "given derivation does not take precedence over existing given factories (e.g. for Lists)"
  ) {
    equalShrinkValues(CaseClassWithListOfCaseClass.expectedShrink)
  }

  property("supports recursive structures") {
    equalShrinkValues(RecursiveList.expectedShrink[Int])
  }

  property("supports recursive structures (across nested sealed traits)") {
    equalShrinkValues(NestedSumsRecursiveList.expectedShrink[Int])
  }

  property("supports recursive structures (across more complex nested structures)") {
    equalShrinkValues(MaybeMaybeList.expectedShrink[Int])
  }

  property("supports direct recursion") {
    equalShrinkValues(DirectRecursion.expectedShrink)
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
