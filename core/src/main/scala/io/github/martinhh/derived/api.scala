package io.github.martinhh.derived

/**
 * Public "API-entry-point" for derivation of `Arbitrary`-instances.
 */
object arbitrary extends ArbitraryDeriving:

  /**
   * Alternative "API-entry-point" for the "retry on stack overflow" behavior of derived `Gen`s.
   *
   * Note that "retry on stack overflow" does not work for Scala JS nor Scala Native. Generally,
   * using this is not recommended. This behavior is only offered to make migration from
   * scalacheck-shapeless easier (as "retry on stack overflow" is the default behavior there).
   */
  object retrying extends StackOverflowCatchingArbitraryDeriving

/**
 * Public "API-entry-point" for derivation of scalacheck-typeclass-instances.
 *
 * This does not provide derivation of `Shrink`-instances as that might not always be desired.
 * You can opt in to derivation of `Shrink`-instances via `shrink`.
 */
object scalacheck extends ArbitraryDeriving with CogenDeriving:

  /**
   * Alternative "API-entry-point" for the "retry on stack overflow" behavior of derived `Gen`s.
   *
   * Note that "retry on stack overflow" does not work for Scala JS nor Scala Native. Generally,
   * using this is not recommended. This behavior is only offered to make migration from
   * scalacheck-shapeless easier (as "retry on stack overflow" is the default behavior there).
   * Note that "retry on stack overflow" does not work for Scala JS nor Scala Native.
   */
  object retrying extends StackOverflowCatchingArbitraryDeriving with CogenDeriving

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
