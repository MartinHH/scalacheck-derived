package io.github.martinhh.derived.extras.union

import scala.reflect.TypeTest

import scala.compiletime.summonInline

// combines an instance of a typeclass with a TypeTest
private case class TypedTypeClass[TC[_], A](typeTest: TypeTest[Any, A], instance: TC[A])

private object TypedTypeClass:
  inline given derived[TC[_], A]: TypedTypeClass[TC, A] =
    TypedTypeClass[TC, A](summonInline[TypeTest[Any, A]], summonInline[TC[A]])
