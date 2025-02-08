package io.github.martinhh

import io.github.martinhh.derived.scalacheck.given

import org.scalacheck.Arbitrary
import org.scalacheck.Cogen

// format: off
case class BigCaseClass(
  a1: Int, b1: Int, c1: Int, d1: Int, e1: Int, f1: Int, g1: Int, h1: Int, i1: Int, j1: Int,
  k1: Int, l1: Int, m1: Int, n1: Int, o1: Int, p1: Int, q1: Int, r1: Int, s1: Int, t1: Int,
  u1: Int, v1: Int, w1: Int, x1: Int, y1: Int, z1: Int,
  a2: Int, b2: Int, c2: Int, d2: Int, e2: Int, f2: Int, g2: Int, h2: Int, i2: Int, j2: Int,
  k2: Int, l2: Int, m2: Int, n2: Int, o2: Int, p2: Int, q2: Int, r2: Int, s2: Int, t2: Int
)
// format: on

/**
 * Tests testing derivation for larger type (with more than 32 MirroredElemTypes).
 *
 * (This is only possible with Scala 3.4.0 or higher).
 */
// TODO: integrate into regular tests once we skip to next LTS (3.7.x)
class LargeElemTypesSuite extends munit.BaseFunSuite:

  test(
    "(with scala 3.4.0 or higher) supports derivation of Arbitrary instances for case classes with more" +
      " than 32 fields (if -Xmax-inlines=32)"
  ) {
    summon[Arbitrary[BigCaseClass]]
  }

  test(
    "(with scala 3.4.0 or higher) supports derivation of Cogen instances for case classes with more" +
      " than 32 fields (if -Xmax-inlines=32)"
  ) {
    summon[Cogen[BigCaseClass]]
  }
