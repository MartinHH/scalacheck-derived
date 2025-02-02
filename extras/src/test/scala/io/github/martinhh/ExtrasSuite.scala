package io.github.martinhh

import io.github.martinhh.derived.extras.union.scalacheck.given
import io.github.martinhh.derived.extras.union.shrink.given
import io.github.martinhh.derived.extras.literal.arbitrary.given
import io.github.martinhh.derived.extras.literal.cogen.given
import io.github.martinhh.derived.arbitrary.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import org.scalacheck.Shrink

class ExtrasSuite extends test.ArbitrarySuite with test.CogenSuite with test.ShrinkSuite:

  test("Arbitrary for union of two types") {
    type TheUnion = String | Int
    val expectedGen = Gen.oneOf[TheUnion](arbitrary[String], arbitrary[Int])
    equalArbitraryValues(expectedGen)
  }

  test("Arbitrary for union of three types") {
    type TheUnion = Boolean | String | Int
    val expectedGen = Gen.oneOf[TheUnion](arbitrary[Boolean], arbitrary[String], arbitrary[Int])
    equalArbitraryValues(expectedGen)
  }

  test("Arbitrary for union of two more complex types") {
    type TheUnion = Option[String] | List[Int]
    val expectedGen = Gen.oneOf[TheUnion](arbitrary[Option[String]], arbitrary[List[Int]])
    equalArbitraryValues(expectedGen)
  }

  test("Arbitrary for union of three more complex types") {
    type TheUnion = Set[Double] | Option[String] | List[Int]
    val expectedGen =
      Gen.oneOf[TheUnion](arbitrary[Set[Double]], arbitrary[Option[String]], arbitrary[List[Int]])
    equalArbitraryValues(expectedGen)
  }

  test("Arbitrary for union of two string literals") {
    type TheUnion = "Foo" | "Bar"
    val expectedGen = Gen.oneOf[TheUnion](Gen.const[TheUnion]("Foo"), Gen.const[TheUnion]("Bar"))
    equalArbitraryValues[TheUnion](expectedGen)
  }

  test("Arbitrary for union of three string literals") {
    type TheUnion = "Foo" | "Bar" | "Baz"
    val expectedGen = Gen.oneOf[TheUnion](
      Gen.const[TheUnion]("Foo"),
      Gen.const[TheUnion]("Bar"),
      Gen.const[TheUnion]("Baz")
    )
    equalArbitraryValues[TheUnion](expectedGen)
  }

  test("Arbitrary for union of two case classes") {
    case class A()
    case class B()
    type TheUnion = A | B
    val expectedGen = Gen.oneOf[TheUnion](Gen.const[TheUnion](A()), Gen.const[TheUnion](B()))
    equalArbitraryValues[TheUnion](expectedGen)
  }

  test("arbUnion[x] fails to compile if x is not a union") {
    assert(compileErrors("arbUnion[Int]").nonEmpty)
    assert(compileErrors("arbUnion[String]").nonEmpty)
    // sanity check:
    assert(compileErrors("arbUnion[Int | String]").isEmpty)
  }

  test("Cogen for union of literal types") {
    type TheUnion = 1 | 2
    val expectedCogen: Cogen[TheUnion] =
      Cogen { (seed, value) =>
        value match
          case 1 =>
            Cogen.perturb(
              Cogen.perturb(seed, 0L),
              0
            )
          case 2 =>
            Cogen.perturb(
              Cogen.perturb(seed, 0L),
              1
            )
      }
    equalCogenValues(expectedCogen)
  }

  test("Cogen for union of three types") {
    type TheUnion = String | Int | Boolean
    val expectedCogen: Cogen[TheUnion] =
      Cogen { (seed, value) =>
        value match
          case s: String =>
            Cogen.perturb(
              Cogen.perturb(
                seed,
                s
              ),
              0
            )
          case i: Int =>
            Cogen.perturb(
              Cogen.perturb(
                seed,
                i
              ),
              1
            )
          case b: Boolean =>
            Cogen.perturb(
              Cogen.perturb(
                seed,
                b
              ),
              2
            )
      }
    equalCogenValues(expectedCogen)
  }

  test("shrinkUnion[x] fails to compile if x is not a union") {
    assert(compileErrors("shrinkUnion[Int]").nonEmpty)
    assert(compileErrors("shrinkUnion[String]").nonEmpty)
    // sanity check:
    assert(compileErrors("shrinkUnion[Int | String]").isEmpty)
  }

  test("Shrink for union of three types") {
    type TheUnion = String | Int | Boolean
    val expectedShrink: Shrink[TheUnion] =
      Shrink { (a: TheUnion) =>
        a match
          case s: String =>
            Shrink.shrinkString.shrink(s)
          case i: Int =>
            Shrink.shrinkIntegral[Int].shrink(i)
          case b: Boolean =>
            Shrink.shrinkAny[Boolean].shrink(b)
      }
    equalShrinkValues(expectedShrink)
  }

  test("cogenUnion[x] fails to compile if x is not a union") {
    assert(compileErrors("cogenUnion[Int]").nonEmpty)
    assert(compileErrors("cogenUnion[String]").nonEmpty)
    // sanity check:
    assert(compileErrors("cogenUnion[Int | String]").isEmpty)
  }
