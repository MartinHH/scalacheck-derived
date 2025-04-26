package io.github.martinhh.derived

import org.scalacheck.Gen

/**
 * Public "API-entry-point" for derivation of `Arbitrary`-instances.
 */
object arbitrary extends DefaultArbitraryDeriving:
  def withSumGenFactory(
    sumGenFactory: [a] => (List[Gen[a]], Option[Gen[a]]) => Gen[a]
  ): ArbitraryDeriving =
    ArbitraryDeriving(sumGenFactory)

/**
 * Public "API-entry-point" for derivation of scalacheck-typeclass-instances.
 *
 * This does not provide derivation of `Shrink`-instances as that might not always be desired.
 * You can opt in to derivation of `Shrink`-instances via `shrink`.
 */
object scalacheck extends DefaultArbitraryDeriving with CogenDeriving

/**
 * Public "API-entry-point" for derivation of `Cogen`-instances.
 */
object cogen extends CogenDeriving

/**
 * Public "API-entry-point" for derivation of `Shrink`-instances.
 *
 * Note that derivation increases compile-time and that there is a (non-shrinking) fallback
 * `Shrink[T]` for any type `T` provided by `org.scalacheck.Shrink.shrinkAny`.
 * So: only use this if your need for shrinking justifies the extra compile-time overhead.
 */
object shrink extends ShrinkDeriving
