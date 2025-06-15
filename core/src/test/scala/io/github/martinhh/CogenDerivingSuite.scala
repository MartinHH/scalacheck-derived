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

  test("deriveCogen uses existing givens for subtypes of sumtypes (instead of deriving them)") {
    import derived.scalacheck.anyGivenArbitrary
    import derived.scalacheck.deriveCogen
    equalCogenValues(ADTWithGivenInstancesForSubtype.expectedCogen)(
      using arbSeed,
      anyGivenArbitrary,
      deriveCogen
    )
  }

  test("deriveCogenShallow succeeds for simple case class") {
    equalCogenValues(SimpleCaseClass.expectedCogen)(
      using summon,
      derived.scalacheck.deriveArbitrary,
      derived.cogen.deriveCogenShallow
    )
  }

  test("deriveCogenShallow fails for case class containing another case class") {
    val error: String =
      compileErrors("derived.cogen.deriveCogenShallow[AbstractSubClass.SubclassA]")
    assert(
      error.contains(
        "No given instance of type org.scalacheck.Cogen[io.github.martinhh.SimpleCaseClass] was found"
      )
    )
  }

  test(
    "deriveCogenShallow succeeds for case class containing another case class if an instance for that is available"
  ) {
    given Cogen[SimpleCaseClass] = SimpleCaseClass.expectedCogen

    equalCogenValues(AbstractSubClass.SubclassA.expectedCogen)(
      using summon,
      derived.scalacheck.deriveArbitrary,
      derived.cogen.deriveCogenShallow
    )
  }

  test("deriveCogenShallow succeeds for ADT if subclasses are derivable via deriveShallow") {
    equalCogenValues(Maybe.expectedCogen[Int])(
      using summon,
      derived.scalacheck.deriveArbitrary,
      derived.cogen.deriveCogenShallow[Maybe[Int]]
    )
  }

  test("deriveCogenShallow fails for ADT if a subclass is not derivable via deriveShallow") {
    val error: String =
      compileErrors("derived.cogen.deriveCogenShallow[Maybe[SimpleCaseClass]]")
    assert(
      error.contains(
        "Derivation failed. No given instance of type Summoner[io.github.martinhh.Maybe.Defined[io.github.martinhh.SimpleCaseClass]] was found." +
          " This is most likely due to no Cogen[io.github.martinhh.Maybe.Defined[io.github.martinhh.SimpleCaseClass]] being available."
      )
    )
  }

  test(
    "deriveCogenShallow succeeds for ADT if a subclass is made derivable via deriveShallow by required instances in scope"
  ) {
    given Cogen[SimpleCaseClass] = SimpleCaseClass.expectedCogen

    equalCogenValues(Maybe.expectedCogen[SimpleCaseClass])(
      using summon,
      derived.scalacheck.deriveArbitrary,
      derived.cogen.deriveCogenShallow[Maybe[SimpleCaseClass]]
    )
  }

  test("deriveCogenShallow prefers existing instance for ADT-subtypes") {
    case class Foo(x: Int)
    val customCogen: Cogen[Maybe.Defined[Foo]] = Cogen(_ => 13L)

    val expectedCogen: Cogen[Maybe[Foo]] =
      Cogen { (seed, value) =>
        value match
          case d: Maybe.Defined[Foo] =>
            Cogen.perturb(
              Cogen.perturb[Maybe.Defined[Foo]](
                seed,
                d
              )(customCogen),
              0
            )
          case Maybe.Undefined =>
            perturbSingletonInSum(1, seed, Maybe.Undefined)
      }
    given Cogen[Maybe.Defined[Foo]] = customCogen
    equalCogenValues(expectedCogen)(
      using summon,
      derived.scalacheck.deriveArbitrary,
      derived.cogen.deriveCogenShallow[Maybe[Foo]]
    )
  }

  test("deriveCogenShallow supports nested sealed traits (with recursion)") {
    equalCogenValues(NestedSumsRecursiveList.expectedCogen[Int])(
      using summon,
      derived.scalacheck.deriveArbitrary,
      derived.cogen.deriveCogenShallow
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

  test("given derivation does not take precedence over existing givens for subtypes of sumtypes") {
    equalCogenValues(ADTWithGivenInstancesForSubtype.expectedCogen)
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
