package io.github.martinhh.derived

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

/**
 * Typeclass that allows to provide a "fallback" for recursive data structures.
 *
 * If an instance of this is in scope, derived `Arbitrary`-instances for sum types will
 * use the provided `Gen` once recursion depth reaches the configured scalacheck `size`.
 *
 * Thus, the provided `Gen` should return a non-recursive subtype of the sum type
 * (e.g. the "leaf" of a recursive tree).
 */
sealed trait RecursionFallback[A]:
  def fallbackGen: Gen[A]

object RecursionFallback:
  def apply[A](genA: Gen[A]): RecursionFallback[A] =
    new RecursionFallback[A]:
      override def fallbackGen: Gen[A] = genA

  def apply[A](a: A): RecursionFallback[A] = apply(Gen.const(a))

  def apply[A, B <: A](using arbB: Arbitrary[B]): RecursionFallback[A] =
    apply(arbB.arbitrary)
