package io.github.martinhh

import io.github.martinhh.derived.scalacheck.given
import io.github.martinhh.derived.shrink.given

import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Shrink

// format: off
case class BigCaseClass(
  a1: Int, b1: Int, c1: Int, d1: Int, e1: Int, f1: Int, g1: Int, h1: Int, i1: Int, j1: Int,
  k1: Int, l1: Int, m1: Int, n1: Int, o1: Int, p1: Int, q1: Int, r1: Int, s1: Int, t1: Int,
  u1: Int, v1: Int, w1: Int, x1: Int, y1: Int, z1: Int,
  a2: Int, b2: Int, c2: Int, d2: Int, e2: Int, f2: Int, g2: Int, h2: Int, i2: Int, j2: Int,
  k2: Int, l2: Int, m2: Int, n2: Int, o2: Int, p2: Int, q2: Int, r2: Int, s2: Int, t2: Int,
  u2: Int, v2: Int, w2: Int, x2: Int, y2: Int, z2: Int,
  a3: Int, b3: Int, c3: Int, d3: Int, e3: Int, f3: Int, g3: Int, h3: Int, i3: Int, j3: Int,
  k3: Int, l3: Int, m3: Int, n3: Int, o3: Int, p3: Int, q3: Int, r3: Int, s3: Int, t3: Int,
  u3: Int, v3: Int, w3: Int, x3: Int, y3: Int, z3: Int
)
// format: on

/**
 * Tests testing derivation for larger type (with more than 32 MirroredElemTypes).
 *
 * (This is only possible with Scala 3.4.0 or higher).
 */
// TODO: integrate into regular tests once we skip to next LTS (3.7.x)
class LargeElemTypesSuite extends test.ArbitrarySuite with test.CogenSuite with test.ShrinkSuite:

  // since we use the derived instances as expected instances, these tests only test that derivation compiles and
  // that no exceptions are thrown at runtime:

  test(
    "(with scala 3.4.0 or higher) supports derivation of Arbitrary instances for case classes with more" +
      " than 32 fields (if -Xmax-inlines=32)"
  ) {
    equalArbitraryValues(summon[Arbitrary[BigCaseClass]].arbitrary)
  }

  test(
    "(with scala 3.4.0 or higher) supports derivation of Cogen instances for case classes with more" +
      " than 32 fields (if -Xmax-inlines=32)"
  ) {
    equalCogenValues(summon[Cogen[BigCaseClass]])
  }

  test(
    "(with scala 3.4.0 or higher) supports derivation of Shrink instances for case classes with more" +
      " than 32 fields (if -Xmax-inlines=32)"
  ) {
    equalShrinkValues(summon[Shrink[BigCaseClass]])
  }
