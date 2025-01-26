package io.github.martinhh.derived.extras.union

import org.scalacheck.Cogen
import org.scalacheck.rng.Seed

private type UnionTypedCogens[A] = UnionTypedTypeClasses[Cogen, A]

private def toCogen[A](utc: UnionTypedCogens[A]): Cogen[A] =
  Cogen { (seed: Seed, a: A) =>
    object TheUnapply:
      def unapply(tc: TypedTypeClass[Cogen, ?]): Option[Seed] =
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

  transparent inline given cogenUnion[X](using inline utc: UnionTypedCogens[X]): Cogen[X] =
    toCogen(utc)
