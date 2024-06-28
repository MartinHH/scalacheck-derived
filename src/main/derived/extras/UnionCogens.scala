package io.github.martinhh.derived.extras

import org.scalacheck.Cogen
import org.scalacheck.rng.Seed

import scala.compiletime.summonInline
import scala.reflect.TypeTest

// combines a Cogen with a TypeTest that allows matching on the type of the Cogen
private case class TypedCogen[A](typeTest: TypeTest[Any, A], cogen: Cogen[A]):
  def tryPerturb(seed: Seed, a: Any): Option[Seed] = typeTest.unapply(a).map(cogen.perturb(seed, _))

private object TypedCogen:
  inline given derived[A]: TypedCogen[A] =
    TypedCogen(summonInline[TypeTest[Any, A]], summonInline[Cogen[A]])

// type for accumulating the TypedCogen-instances of a union
private sealed trait TypedCogens[A]:
  def instances: List[TypedCogen[?]]

private object TypedCogens:
  inline given derived[A]: SingleTypedCogens[A] =
    SingleTypedCogens(summonInline[TypedCogen[A]])

private case class SingleTypedCogens[A](instance: TypedCogen[A]) extends TypedCogens[A]:
  override def instances: List[TypedCogen[?]] = List(instance)

private case class UnionTypedCogens[A](instances: List[TypedCogen[?]]) extends TypedCogens[A]:
  def toCogen: Cogen[A] =
    Cogen { (seed: Seed, a: A) =>
      object TheUnapply:
        def unapply(typedCogen: TypedCogen[?]): Option[Seed] = typedCogen.tryPerturb(seed, a)
      val seedOpt = instances.zipWithIndex.collectFirst { case (TheUnapply(seed), i) =>
        Cogen.perturb(seed, i)
      }
      assert(seedOpt.isDefined, "This case should be unreachable")
      seedOpt.get
    }

private trait UnionCogens:
  transparent inline given unionTypedCogensMacro[X]: UnionTypedCogens[X] =
    io.github.martinhh.derived.extras.unionTypedCogensMacro

  transparent inline given cogenUnion[X](using inline bg: UnionTypedCogens[X]): Cogen[X] =
    bg.toCogen
