package io.github.martinhh.derived.extras.union

import org.scalacheck.Cogen
import org.scalacheck.rng.Seed

import scala.annotation.implicitNotFound

private type TypedCogen[A] = TypedTypeClass[Cogen, A]

private type UnionTypedCogens[A] = UnionTypeClasses[TypedCogen, A]

private def toCogen[A](utc: UnionTypedCogens[A]): Cogen[A] =
  Cogen { (seed: Seed, a: A) =>
    object TheUnapply:
      def unapply(iw: InstanceWrapper[TypedCogen, ? <: A]): Option[Seed] =
        val tc: TypedCogen[? <: A] = iw.instance
        tc.typeTest.unapply(a).map(tc.instance.perturb(seed, _))
    val seedOpt = utc.instances.zipWithIndex.collectFirst { case (TheUnapply(seed), i) =>
      Cogen.perturb(seed, i)
    }
    assert(seedOpt.isDefined, "This case should be unreachable")
    seedOpt.get
  }

private trait UnionCogens:
  transparent inline given unionTypedCogensMacro[X]: UnionTypedCogens[X] =
    io.github.martinhh.derived.extras.union.unionTypedCogensMacro

  transparent inline given cogenUnion[X](
    using @implicitNotFound(
      "Could not find a given instance for UnionTypedCogens[${X}].\n" +
        "Reason might be that ${X} is not a union or (if ${X} is a union)\n" +
        "that there is no given instance for one of its type member"
    ) inline utc: UnionTypedCogens[X]
  ): Cogen[X] =
    toCogen(utc)
