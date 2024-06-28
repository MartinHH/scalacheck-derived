package io.github.martinhh

import io.github.martinhh.derived.extras.TypedCogen
import io.github.martinhh.derived.extras.all.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

class ExtrasSuite extends ArbitrarySuite with CogenSuite:

  test("Arbitrary for union of two types") {
    type TheUnion = String | Int
    val expectedGen = Gen.oneOf[TheUnion](arbitrary[String], arbitrary[Int])
    equalValues(expectedGen)
  }

  test("Arbitrary for union of three types") {
    type TheUnion = Boolean | String | Int
    val expectedGen = Gen.oneOf[TheUnion](arbitrary[Boolean], arbitrary[String], arbitrary[Int])
    equalValues(expectedGen)
  }

  test("Arbitrary for union of two more complex types") {
    type TheUnion = Option[String] | List[Int]
    val expectedGen = Gen.oneOf[TheUnion](arbitrary[Option[String]], arbitrary[List[Int]])
    equalValues(expectedGen)
  }

  test("Arbitrary for union of three more complex types") {
    type TheUnion = Set[Double] | Option[String] | List[Int]
    val expectedGen =
      Gen.oneOf[TheUnion](arbitrary[Set[Double]], arbitrary[Option[String]], arbitrary[List[Int]])
    equalValues(expectedGen)
  }

  test("Arbitrary for union of two string literals") {
    import io.github.martinhh.derived.extras.literal.given
    type TheUnion = "Foo" | "Bar"
    val expectedGen = Gen.oneOf[TheUnion](Gen.const[TheUnion]("Foo"), Gen.const[TheUnion]("Bar"))
    equalValues[TheUnion](expectedGen)
  }

  test("Arbitrary for union of three string literals") {
    import io.github.martinhh.derived.extras.literal.given
    type TheUnion = "Foo" | "Bar" | "Baz"
    val expectedGen = Gen.oneOf[TheUnion](
      Gen.const[TheUnion]("Foo"),
      Gen.const[TheUnion]("Bar"),
      Gen.const[TheUnion]("Baz")
    )
    equalValues[TheUnion](expectedGen)
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
    equalValues(expectedCogen)
  }
