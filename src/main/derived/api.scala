package io.github.martinhh.derived

/**
 * Public "API-entry-point" for derivation of `Arbitrary`-instances.
 */
object arbitrary extends ArbitraryDeriving

/**
 * Public "API-entry-point" for derivation of scalacheck-typeclass-instances.
 */
object scalacheck extends ArbitraryDeriving with CogenDeriving
