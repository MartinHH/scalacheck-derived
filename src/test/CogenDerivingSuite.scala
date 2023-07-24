package io.github.martinhh

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.rng.Seed

class CogenDerivingSuite extends munit.ScalaCheckSuite:

  private def equalValues[T](
    expectedCogen: Cogen[T]
  )(using arbSeed: Arbitrary[Seed], arbT: Arbitrary[T], derivedCogen: Cogen[T]): Prop =
    Prop.forAll { (s: Seed, t: T) =>
      assertEquals(derivedCogen.perturb(s, t), expectedCogen.perturb(s, t))
    }

  import CogenDerivingSuite.arbSeed

  test("deriveCogen allows to derive a given without loop of given definition") {
    given cogen: Cogen[SimpleCaseClass] = derived.scalacheck.deriveCogen
    // proven if it compiles, but let's do some run-time testing anyway:
    import derived.scalacheck.anyGivenArbitrary
    equalValues(SimpleCaseClass.expectedCogen)
  }

  import derived.scalacheck.given

  property("perturbs to same seeds as non-derived expected Cogen (for simple ADT)") {
    equalValues(SimpleADT.expectedCogen)
  }

  property("perturbs to same seeds as non-derived expected Cogen (for simple Enum)") {
    equalValues(ABC.expectedCogen)
  }

  property(
    "perturbs to same seeds as non-derived expected Cogen (for case class with containers)"
  ) {
    equalValues(CaseClassWithContainers.expectedCogen)
  }

  test("given derivation does not take precedence over existing givens") {
    equalValues(HasGivenInstances.specialHasGivenInstancesCogen)
  }

  test("given derivation of child-instances does not take precedence over existing givens") {
    equalValues(HasMemberThatHasGivenInstances.expectedCogen)
  }

  test("enables derivation of Arbitrary instances for functions") {
    val arbFunction1: Arbitrary[ComplexADTWithNestedMembers => ABC] =
      summon
    val arbFunction2: Arbitrary[(ComplexADTWithNestedMembers, ABC) => CaseClassWithContainers] =
      summon
    // proven because it compiles
  }

  // (should support as least as many fields as ArbitraryDeriving)
  test("supports case classes with up to 27 fields (if -Xmax-inlines=32)") {
    summon[Cogen[MaxCaseClass]]
  }

  // (should support as least as many fields as ArbitraryDeriving)
  test("supports enums with up to 25 members (if -Xmax-inlines=32)") {
    summon[Cogen[MaxEnum]]
  }

object CogenDerivingSuite:
  private given arbSeed: Arbitrary[Seed] = Arbitrary(arbitrary[Long].map(Seed.apply))
