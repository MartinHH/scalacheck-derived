package io.github.martinhh.derived.extras.union

import io.github.martinhh.derived.genOneOf

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

import scala.compiletime.summonInline

// Serves more or less the same purpose as io.github.martinhh.derived.Gens (just in the context of unions).
// Using a separate type here (instead of reusing Gens) is intended to reduce the compile-time for implicit resolution.
private case class UnionGens[+A](gens: List[Gen[A]])

private object UnionGens:
  inline given derived[A]: UnionGens[A] =
    UnionGens(List(summonInline[Arbitrary[A]].arbitrary))

private trait UnionArbitraries:

  transparent inline given unionGensMacro[X]: UnionGens[X] =
    io.github.martinhh.derived.extras.union.unionGensMacro

  transparent inline given arbUnion[X](using inline bg: UnionGens[X]): Arbitrary[X] =
    Arbitrary(genOneOf(bg.gens))
