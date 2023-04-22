package io.github.martinhh

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Gen.Parameters
import org.scalacheck.rng.Seed

class ArbitraryDerivingSuite extends munit.FunSuite:

  private def equalValues[T](
    expectedGen: Gen[T],
    nTests: Int = 100
  )(using derivedArb: Arbitrary[T]): Unit =
    (0 until nTests).foldLeft(Seed.random()) { case (seed, _) =>
      val expected = expectedGen(Parameters.default, seed)
      val derived = derivedArb.arbitrary(Parameters.default, seed)
      assertEquals(derived, expected, s"Differing values for seed $seed")
      seed.next
    }

  test("deriveArb allows to derive a given without loop of given definition") {
    given arb: Arbitrary[SimpleCaseClass] = derived.arbitrary.deriveArbitrary
    // proven if it compiles, but let's do some run-time testing anyway:
    equalValues(SimpleCaseClass.expectedGen, nTests = 10)(using arb)
  }

  import io.github.martinhh.derived.arbitrary.given

  test("Generates same values as non-derived Gen (for simple case class)") {
    equalValues(SimpleCaseClass.expectedGen)
  }

  test(
    "Generates same values as non-derived Gen (for case class with containers)"
  ) {
    equalValues(CaseClassWithContainers.expectedGen)
  }

  test("Generates same values as non-derived Gen (for simple ADT)") {
    equalValues(SimpleADT.expectedGen)
  }

  test("Generates same values as non-derived Gen (for simple Enum)") {
    equalValues(ABC.expectedGen)
  }

  test("Generates same values as non-derived Gen (for more complex example)") {
    equalValues(ComplexADTWithNestedMembers.expectedGen)
  }

  test("Gives precedence to any arbitraries in scope") {
    given arb: Arbitrary[SimpleADT] = Arbitrary(SimpleCaseClass.expectedGen)
    val expectedGen: Gen[AbstractSubClass.SubclassB] =
      arb.arbitrary.map(AbstractSubClass.SubclassB.apply)
    equalValues(expectedGen)
  }

  // not a hard requirement (just guarding against accidental worsening by refactoring)
  test("supports case classes with up to 27 fields (if -Xmax-inlines=32)") {
    summon[Arbitrary[MaxCaseClass]]
  }

  // not a hard requirement (just guarding against accidental worsening by refactoring)
  test("supports enums with up to 25 members (if -Xmax-inlines=32)") {
    summon[Arbitrary[MaxEnum]]
  }
