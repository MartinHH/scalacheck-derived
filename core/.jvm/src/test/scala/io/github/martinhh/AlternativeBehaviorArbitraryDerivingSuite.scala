package io.github.martinhh

import org.scalacheck
import org.scalacheck.Arbitrary
import derived.scalacheck.retrying.deriveArbitrary

import org.scalacheck.Gen
import org.scalacheck.Prop

class AlternativeBehaviorArbitraryDerivingSuite
  extends test.ArbitrarySuite
     with munit.ScalaCheckSuite:

  property("tree-like data structures do not make tests crash with stack overflow (on JVM only)") {
    given arbitraryTree: Arbitrary[Tree] = deriveArbitrary
    Prop.forAll { (_: Tree) =>
      true
    }
  }

  test("tree-like data structures lead to the expected gen") {
    import derived.arbitrary.retrying.given
    equalArbitraryValues(Tree.expectedGen)
  }
