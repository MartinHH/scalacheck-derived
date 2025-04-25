package io.github.martinhh

import io.github.martinhh.derived.RecursionFallback
import org.scalacheck
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Gen.Parameters

class ArbitraryDerivingSuite extends test.ArbitrarySuite:

  test("deriveArbitrary allows to derive a given without loop of given definition") {
    given arb: Arbitrary[SimpleCaseClass] = derived.arbitrary.deriveArbitrary
    // proven if it compiles, but let's do some run-time testing anyway:
    equalArbitraryValues(SimpleCaseClass.expectedGen, nTests = 10)(using arb)
  }

  test("deriveArbitrary supports recursive structures") {
    equalArbitraryValues(RecursiveList.expectedGen[Int])(using derived.arbitrary.deriveArbitrary)
    equalArbitraryValues(NestedSumsRecursiveList.expectedGen[Int])(
      using derived.arbitrary.deriveArbitrary
    )
    equalArbitraryValues(MaybeMaybeList.expectedGen[Int])(using derived.arbitrary.deriveArbitrary)
  }

  test("deriveArbitrary uses existing given factories (e.g. for Lists)") {
    equalArbitraryValues(CaseClassWithListOfCaseClass.expectedGen)(
      using derived.arbitrary.deriveArbitrary
    )
  }

  import io.github.martinhh.derived.arbitrary.given

  test("Generates same values as non-derived Gen (for simple case class)") {
    equalArbitraryValues(SimpleCaseClass.expectedGen)
  }

  test(
    "Generates same values as non-derived Gen (for case class with containers)"
  ) {
    equalArbitraryValues(CaseClassWithContainers.expectedGen)
  }

  test("Generates same values as non-derived Gen (for simple ADT)") {
    equalArbitraryValues(SimpleADT.expectedGen)
  }

  test("Generates same values as non-derived Gen (for simple Enum)") {
    equalArbitraryValues(ABC.expectedGen)
  }

  test("Generates same values as non-derived Gen (for more complex example)") {
    equalArbitraryValues(ComplexADTWithNestedMembers.expectedGen)
  }

  test("Gives precedence to any arbitraries in local scope") {
    given arb: Arbitrary[SimpleADT] = Arbitrary(SimpleCaseClass.expectedGen)
    val expectedGen: Gen[AbstractSubClass.SubclassB] =
      arb.arbitrary.map(AbstractSubClass.SubclassB.apply)
    equalArbitraryValues(expectedGen)
  }

  test("given derivation does not take precedence over existing givens") {
    equalArbitraryValues(HasGivenInstances.specialHasGivenInstancesArbitrary.arbitrary)
  }

  test("given derivation of child-instances does not take precedence over existing givens") {
    equalArbitraryValues(HasMemberThatHasGivenInstances.expectedGen)
  }

  test(
    "given derivation does not take precedence over existing given factories (e.g. for Lists)"
  ) {
    equalArbitraryValues(CaseClassWithListOfCaseClass.expectedGen)
  }

  test("supports recursive structures") {
    equalArbitraryValues(RecursiveList.expectedGen[Int])
  }

  test("supports recursive structures (across nested sealed traits)") {
    equalArbitraryValues(NestedSumsRecursiveList.expectedGen[Int])
  }

  test("supports recursive structures (across more complex nested structures)") {
    equalArbitraryValues(MaybeMaybeList.expectedGen[Int])
  }

  test("supports direct recursion)") {
    equalArbitraryValues(DirectRecursion.expectedGen)
  }

  test("supports tree-like data structures (without blowing the stack)") {
    equalArbitraryValues(Tree.expectedGen)
  }

  test("with size 0 and RecursionFallback, the fallback is used always") {
    val fallback: Gen[Tree] = Gen.const(Tree.Leaf(4242))
    given RecursionFallback[Tree] = RecursionFallback(fallback)
    equalArbitraryValues(fallback, Parameters.default.withSize(0))
  }

  test("supports RecursionFallback.apply(constValue)") {
    val fallback = Tree.Leaf(42)
    given RecursionFallback[Tree] = RecursionFallback(fallback)
    equalArbitraryValues(
      Tree.expectedGenWithFallback(Gen.const(fallback)),
      Parameters.default.withSize(5)
    )
  }

  test("supports RecursionFallback.apply(gen)") {
    val fallback = Gen.choose(13, 16).map(Tree.Leaf.apply)
    given RecursionFallback[Tree] = RecursionFallback(fallback)
    equalArbitraryValues(Tree.expectedGenWithFallback(fallback), Parameters.default.withSize(5))
  }

  test("supports RecursionFallback.apply[Tree, Leaf]") {
    given RecursionFallback[Tree] = RecursionFallback[Tree, Tree.Leaf]
    equalArbitraryValues(
      Tree.expectedGenWithFallback(summon[Arbitrary[Tree.Leaf]].arbitrary),
      Parameters.default.withSize(5)
    )
  }

  test("even distribution in sealed traits with diamond inheritance") {
    equalArbitraryValues(SealedDiamond.expectedGen)
  }

  // not a hard requirement (just guarding against accidental worsening by refactoring)
  test("supports case classes with up to 26 fields (if -Xmax-inlines=32)") {
    summon[Arbitrary[MaxCaseClass]]
  }

  // not a hard requirement (just guarding against accidental worsening by refactoring)
  test("supports enums with up to 24 members (if -Xmax-inlines=32)") {
    summon[Arbitrary[MaxEnum]]
  }
