package io.github.martinhh.derived.extras.union

import scala.reflect.TypeTest

import scala.compiletime.summonInline

// combines an instance of a typeclass with a TypeTest
private case class TypedTypeClass[TC[_], A](typeTest: TypeTest[Any, A], instance: TC[A])

private object TypedTypeClass:
  inline given derived[TC[_], A]: TypedTypeClass[TC, A] =
    TypedTypeClass[TC, A](summonInline[TypeTest[Any, A]], summonInline[TC[A]])

// type for accumulating TypedTypeClass instances (e.g. of a union)
private sealed trait TypedTypeClasses[TC[_], A]:
  def instances: List[TypedTypeClass[TC, ? <: A]]

private object TypedTypeClasses:
  inline given derived[TC[_], A]: SingleTypedTypeClasses[TC, A] =
    SingleTypedTypeClasses(summonInline[TypedTypeClass[TC, A]])

private case class SingleTypedTypeClasses[TC[_], A](instance: TypedTypeClass[TC, A])
  extends TypedTypeClasses[TC, A]:
  override def instances: List[TypedTypeClass[TC, A]] = List(instance)

private case class UnionTypedTypeClasses[TC[_], A](instances: List[TypedTypeClass[TC, ? <: A]])
  extends TypedTypeClasses[TC, A]
