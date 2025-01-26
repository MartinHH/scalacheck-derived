package io.github.martinhh.derived.extras

object union:

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

object literal:
  /**
   * Public "API-entry-point" for derivation of `Arbitrary`-instances for literal types.
   *
   * (No derivation for `Shrink` is provided because there is no sense in shrinking single-value types.
   * No derivation for `Cogen` is provided because the `Cogen` instances for the respective parent types
   * should be sufficient.)
   */
  object arbitrary extends LiteralArbitraries
