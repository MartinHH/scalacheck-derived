package io.github.martinhh.derived.extras.union

/**
 * Public "API-entry-point" for derivation of `Arbitrary`-instances only for union types.
 */
object arbitrary extends UnionArbitraries

/**
 * Public "API-entry-point" for derivation of scalacheck-typeclass-instances for union types.
 *
 * This does not provide derivation of `Shrink`-instances as that might not always be desired.
 * You can opt in to derivation of `Shrink`-instances via `shrink`.
 */
object scalacheck extends UnionArbitraries with UnionCogens

/**
 * Public "API-entry-point" for derivation of `Cogen`-instances only for union types.
 */
object cogen extends UnionCogens

/**
 * Public "API-entry-point" for derivation of `Shrink`-instances only for union types.
 */
object shrink extends UnionShrinks
