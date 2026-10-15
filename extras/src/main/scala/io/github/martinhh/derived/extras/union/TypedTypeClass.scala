package io.github.martinhh.derived.extras.union

import scala.reflect.TypeTest

import scala.compiletime.summonInline

/**
 * Combines an instance of a typeclass with a TypeTest.
 *
 * ATTENTION: this not meant to be treated as public API - do not use this directly,
 * it is only public because otherwise, implicit resolution would fail.
 */
case class TypedTypeClass[TC[_], A](typeTest: TypeTest[Any, A], instance: TC[A])

/**
 * ATTENTION: this not meant to be treated as public API - do not use this directly,
 * it is only public because otherwise, implicit resolution would fail.
 */
object TypedTypeClass:
  inline given derived[TC[_], A]: TypedTypeClass[TC, A] =
    TypedTypeClass[TC, A](summonInline[TypeTest[Any, A]], summonInline[TC[A]])
