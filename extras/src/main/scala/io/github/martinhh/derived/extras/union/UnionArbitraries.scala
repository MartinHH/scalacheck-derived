package io.github.martinhh.derived.extras.union

import io.github.martinhh.derived.genOneOf

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

import scala.annotation.implicitNotFound

private type UnionArbs[A] = UnionTypeClasses[Arbitrary, A]

private def toGen[A](utc: UnionArbs[A]): Gen[A] =
  genOneOf(utc.instances.map(_.instance.arbitrary))

trait UnionArbitraries:

  transparent inline final given unionGensMacro[X]: UnionArbs[X] =
    io.github.martinhh.derived.extras.union.unionTypedGensMacro[X]

  transparent inline final given arbUnion[X](
    using @implicitNotFound(
      "Could not find a given instance for UnionArbs[${X}].\n" +
        "Reason might be that ${X} is not a union or (if ${X} is a union)\n" +
        "that there is no given instance for one of its type member"
    ) inline bg: UnionArbs[X]
  ): Arbitrary[X] =
    Arbitrary(toGen(bg))
