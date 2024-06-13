package io.github.martinhh

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

class UnionExtrasSuite extends ArbitrarySuite:

  test("Arbitrary for union of two types") {
    import io.github.martinhh.derived.extras.union.given
    type TheUnion = String | Int
    val expectedGen = Gen.oneOf[TheUnion](arbitrary[String], arbitrary[Int])
    equalValues(expectedGen)
  }

  test("Arbitrary for union of three types") {
    import io.github.martinhh.derived.extras.union.given
    type TheUnion = Boolean | String | Int
    val expectedGen = Gen.oneOf[TheUnion](arbitrary[Boolean], arbitrary[String], arbitrary[Int])
    equalValues(expectedGen)
  }
