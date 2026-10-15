package io.github.martinhh.derived.extras.union

// wrapper to mitigate "unreducible application of higher-kinded type" error
// - see "FooWrapper":
// https://docs.scala-lang.org/scala3/guides/migration/incompat-other-changes.html#wildcard-type-argument
case class InstanceWrapper[TC[_], A](instance: TC[A])

/**
 * type for accumulating TypedTypeClass instances (e.g. of a union).
 *
 * ATTENTION: this not meant to be treated as public API - do not use this directly,
 * it is only public because otherwise, implicit resolution would fail.
 */
sealed trait TypeClasses[TC[_], A]:
  def instances: List[InstanceWrapper[TC, ? <: A]]

/**
 * ATTENTION: this not meant to be treated as public API - do not use this directly,
 * it is only public because otherwise, implicit resolution would fail.
 */
object TypeClasses:
  inline given derived[TC[_], A](using inline i: TC[A]): SingleTypeClasses[TC, A] =
    SingleTypeClasses(i)

/**
 * ATTENTION: this not meant to be treated as public API - do not use this directly,
 * it is only public because otherwise, implicit resolution would fail.
 */
case class SingleTypeClasses[TC[_], A](instance: TC[A]) extends TypeClasses[TC, A]:
  override def instances: List[InstanceWrapper[TC, ? <: A]] = List(InstanceWrapper(instance))

/**
 * ATTENTION: this not meant to be treated as public API - do not use this directly,
 * it is only public because otherwise, implicit resolution would fail.
 */
case class UnionTypeClasses[TC[_], A](instances: List[InstanceWrapper[TC, ? <: A]])
  extends TypeClasses[TC, A]
