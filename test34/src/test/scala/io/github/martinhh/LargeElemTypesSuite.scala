package io.github.martinhh

import io.github.martinhh.derived.scalacheck.given
import io.github.martinhh.derived.shrink.given

import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Shrink

enum BigEnum:
  case A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1,
    W1, X1, Y1, Z1,
    A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2,
    W2, X2, Y2, Z2

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
      " than 32 fields (while -Xmax-inlines=32)"
  ) {
    equalArbitraryValues(summon[Arbitrary[BigCaseClass]].arbitrary)
  }

  test(
    "(with scala 3.4.0 or higher) supports derivation of Cogen instances for case classes with more" +
      " than 32 fields (while -Xmax-inlines=32)"
  ) {
    equalCogenValues(summon[Cogen[BigCaseClass]])
  }

  test(
    "(with scala 3.4.0 or higher) supports derivation of Shrink instances for case classes with more" +
      " than 32 fields (while -Xmax-inlines=32)"
  ) {
    equalShrinkValues(summon[Shrink[BigCaseClass]])
  }

  test(
    "(with scala 3.4.0 or higher) supports derivation of Arbitrary instances for enum with more" +
      " than 32 members (while -Xmax-inlines=32)"
  ) {
    equalArbitraryValues(summon[Arbitrary[BigEnum]].arbitrary)
  }
