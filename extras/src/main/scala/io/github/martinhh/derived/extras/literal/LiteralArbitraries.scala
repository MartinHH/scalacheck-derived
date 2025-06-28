package io.github.martinhh.derived.extras.literal

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

import scala.compiletime.summonInline

trait LiteralArbitraries:

  final given arbLiteral[A](using v: ValueOf[A]): Arbitrary[A] = Arbitrary(Gen.const(v.value))
