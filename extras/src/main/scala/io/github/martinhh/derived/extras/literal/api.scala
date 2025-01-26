package io.github.martinhh.derived.extras.literal

/**
 * Public "API-entry-point" for derivation of `Arbitrary`-instances for literal types.
 *
 * (No derivation for `Shrink` is provided because there is no sense in shrinking single-value types.
 * No derivation for `Cogen` is provided because the `Cogen` instances for the respective parent types
 * should be sufficient.)
 */
object arbitrary extends LiteralArbitraries
