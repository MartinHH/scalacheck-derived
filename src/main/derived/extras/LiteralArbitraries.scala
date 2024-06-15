package io.github.martinhh.derived.extras

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

import scala.compiletime.summonInline

private trait LiteralArbitraries:

  given arbLiteral[A](using v: ValueOf[A]): Arbitrary[A] = Arbitrary(Gen.const(v.value))
