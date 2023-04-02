package io.github.martinhh.derived

/**
 * Public "API-entry-point" for derivation of `Arbitrary`-instances.
 */
object arbitrary extends ArbitraryDeriving

/**
 * Public "API-entry-point" for derivation of scalacheck-typeclass-instances.
 *
 * (Currently, only `Arbitrary` is supported. But in future versions of this library, this may
 * become the single entry-point for derivation of all of them.)
 */
object scalacheck extends ArbitraryDeriving
