package io.github.martinhh

import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import org.scalacheck.rng.Seed

class CogenDerivingSuite extends test.CogenSuite:

  test("deriveCogen allows to derive a given without loop of given definition") {
    given cogen: Cogen[SimpleCaseClass] = derived.scalacheck.deriveCogen
    // proven if it compiles, but let's do some run-time testing anyway:
    import derived.scalacheck.anyGivenArbitrary
    equalCogenValues(SimpleCaseClass.expectedCogen)
  }

  test("deriveCogen supports recursive structures") {
    import derived.scalacheck.anyGivenArbitrary
    import derived.scalacheck.deriveCogen
    equalCogenValues(RecursiveList.expectedCogen[Int])(
      using arbSeed,
      anyGivenArbitrary,
      deriveCogen
    )
    equalCogenValues(NestedSumsRecursiveList.expectedCogen[Int])(
      using arbSeed,
      anyGivenArbitrary,
      deriveCogen
    )
    equalCogenValues(MaybeMaybeList.expectedCogen[Int])(
      using arbSeed,
      anyGivenArbitrary,
      deriveCogen
    )
  }

  test("deriveCogen uses existing given factories (e.g. for Lists)") {
    import derived.scalacheck.anyGivenArbitrary
    import derived.scalacheck.deriveCogen
    equalCogenValues(CaseClassWithListOfCaseClass.expectedCogen)(
      using arbSeed,
      anyGivenArbitrary,
      deriveCogen
    )
  }

  import derived.scalacheck.given

  property("perturbs to same seeds as non-derived expected Cogen (for simple ADT)") {
    equalCogenValues(SimpleADT.expectedCogen)
  }

  property("perturbs to same seeds as non-derived expected Cogen (for simple Enum)") {
    equalCogenValues(ABC.expectedCogen)
  }

  property(
    "perturbs to same seeds as non-derived expected Cogen (for case class with containers)"
  ) {
    equalCogenValues(CaseClassWithContainers.expectedCogen)
  }

  test("given derivation does not take precedence over existing givens") {
    equalCogenValues(HasGivenInstances.specialHasGivenInstancesCogen)
  }

  test("given derivation of child-instances does not take precedence over existing givens") {
    equalCogenValues(HasMemberThatHasGivenInstances.expectedCogen)
  }

  test(
    "given derivation does not take precedence over existing given factories (e.g. for Lists)"
  ) {
    equalCogenValues(CaseClassWithListOfCaseClass.expectedCogen)
  }

  test("given derivation supports recursive structures") {
    equalCogenValues(RecursiveList.expectedCogen[Int])
  }

  test("given derivation supports recursive structures (across nested sealed traits)") {
    equalCogenValues(NestedSumsRecursiveList.expectedCogen[Int])
  }

  test("given derivation supports recursive structures (across more complex nested structures)") {
    equalCogenValues(MaybeMaybeList.expectedCogen[Int])
  }

  test("given derivation supports direct recursion") {
    equalCogenValues(DirectRecursion.expectedCogen)
  }

  test("enables derivation of Arbitrary instances for functions") {
    val arbFunction1: Arbitrary[ComplexADTWithNestedMembers => ABC] =
      summon
    val arbFunction2: Arbitrary[(ComplexADTWithNestedMembers, ABC) => CaseClassWithContainers] =
      summon
    // proven because it compiles
  }

  test("error message for non derivable members of sum type") {
    val error: String = compileErrors("summon[Cogen[SumWithNonDerivableMember]]")
    // the exact wording is not a hard requirement...
    assert(
      error.contains(
        "This is most likely due to no Cogen[io.github.martinhh.SumWithNonDerivableMember.NonDerivableMember] being available."
      )
    )
  }

  // (should support as least as many fields as ArbitraryDeriving)
  test("supports case classes with up to 26 fields (if -Xmax-inlines=32)") {
    summon[Cogen[MaxCaseClass]]
  }

  // (should support as least as many fields as ArbitraryDeriving)
  test("supports enums with up to 24 members (if -Xmax-inlines=32)") {
    summon[Cogen[MaxEnum]]
  }
