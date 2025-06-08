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

  test("deriveArbitrary uses existing givens for subtypes of sumtypes (instead of deriving them)") {
    equalArbitraryValues(ADTWithGivenInstancesForSubtype.expectedGen)(
      using derived.arbitrary.deriveArbitrary
    )
  }

  test(
    "deriveArbitrary uses existing givens for subtraits of sealed traits (instead of deriving them)"
  ) {
    equalArbitraryValues(SealedTraitWithGivenInstanceForSubtrait.expectedGen)(
      using derived.arbitrary.deriveArbitrary
    )
  }

  test("deriveArbitraryShallow succeeds for simple case class") {
    equalArbitraryValues(SimpleCaseClass.expectedGen)(
      using derived.arbitrary.deriveArbitraryShallow
    )
  }

  test("deriveArbitraryShallow fails for case class containing another case class") {
    val error: String =
      compileErrors("derived.arbitrary.deriveArbitraryShallow[AbstractSubClass.SubclassA]")
    assert(
      error.contains(
        "No given instance of type org.scalacheck.Arbitrary[io.github.martinhh.SimpleCaseClass] was found."
      )
    )
  }

  test(
    "deriveArbitraryShallow succeeds for case class containing another case class if an instance for that is available"
  ) {
    given Arbitrary[SimpleCaseClass] = Arbitrary(SimpleCaseClass.expectedGen)
    equalArbitraryValues(AbstractSubClass.SubclassA.expectedGen)(
      using derived.arbitrary.deriveArbitraryShallow
    )
  }

  test("deriveArbitraryShallow succeeds for ADT if subclasses are derivable via deriveShallow") {
    equalArbitraryValues(Maybe.expectedGen[Int])(
      using derived.arbitrary.deriveArbitraryShallow[Maybe[Int]]
    )
  }

  test("deriveArbitraryShallow fails for ADT if a subclass is not derivable via deriveShallow") {
    val error: String =
      compileErrors("derived.arbitrary.deriveArbitraryShallow[Maybe[SimpleCaseClass]]")
    assert(
      error.contains(
        "Derivation failed. No given instance of type Summoner[io.github.martinhh.Maybe.Defined[io.github.martinhh.SimpleCaseClass]] was found." +
          " This is most likely due to no Arbitrary[io.github.martinhh.Maybe.Defined[io.github.martinhh.SimpleCaseClass]] being available."
      )
    )
  }

  test(
    "deriveArbitraryShallow succeeds for ADT if a subclass is made derivable via deriveShallow by required instances in scope"
  ) {
    given Arbitrary[SimpleCaseClass] = Arbitrary(SimpleCaseClass.expectedGen)
    equalArbitraryValues(Maybe.expectedGen[SimpleCaseClass])(
      using derived.arbitrary.deriveArbitraryShallow[Maybe[SimpleCaseClass]]
    )
  }

  test("deriveArbitraryShallow prefers existing instance for ADT-subtypes") {
    case class Foo(x: Int)
    val customGen: Gen[Maybe.Defined[Foo]] = Gen.const(Maybe.Defined(Foo(35)))
    given Arbitrary[Maybe.Defined[Foo]] = Arbitrary(customGen)
    val expectedGen: Gen[Maybe[Foo]] =
      expectedGenOneOf(
        customGen,
        Gen.const(Maybe.Undefined)
      )
    equalArbitraryValues(expectedGen)(
      using derived.arbitrary.deriveArbitraryShallow[Maybe[Foo]]
    )
  }

  test("deriveArbitraryShallow supports recursive sum types") {
    equalArbitraryValues(Tree.expectedGen)(
      using derived.arbitrary.deriveArbitraryShallow
    )
  }

  test("deriveArbitraryShallow supports nested sealed traits (with diamond inheritance)") {
    equalArbitraryValues(SealedDiamond.expectedGen)(
      using derived.arbitrary.deriveArbitraryShallow
    )
  }

  test("deriveArbitraryShallow supports nested sealed traits (with recursion)") {
    equalArbitraryValues(NestedSumsRecursiveList.expectedGen[Int])(
      using derived.arbitrary.deriveArbitraryShallow
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

  test("given derivation does not take precedence over existing givens for subtypes of sumtypes") {
    equalArbitraryValues(ADTWithGivenInstancesForSubtype.expectedGen)
  }

  test(
    "given derivation does not take precedence over existing givens for subtraits of sealed traits"
  ) {
    equalArbitraryValues(SealedTraitWithGivenInstanceForSubtrait.expectedGen)
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

  test("error message for non derivable members of sum type") {
    val error: String = compileErrors("summon[Arbitrary[SumWithNonDerivableMember]]")
    // the exact wording is not a hard requirement...
    assert(
      error.contains(
        "This is most likely due to no Arbitrary[io.github.martinhh.SumWithNonDerivableMember.NonDerivableMember] being available."
      )
    )
  }

  // not a hard requirement (just guarding against accidental worsening by refactoring)
  test("supports case classes with up to 26 fields (if -Xmax-inlines=32)") {
    summon[Arbitrary[MaxCaseClass]]
  }

  // not a hard requirement (just guarding against accidental worsening by refactoring)
  test("supports enums with up to 24 members (if -Xmax-inlines=32)") {
    summon[Arbitrary[MaxEnum]]
  }
