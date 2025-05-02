package io.github.martinhh.derived

import org.scalacheck.Gen

/**
 * Public "API-entry-point" for derivation of `Arbitrary`-instances.
 */
object arbitrary extends DefaultArbitraryDeriving:

  /**
   * Contains various factories for customizing derivation.
   *
   * Provides support for configuring how the `Gen`-instances of the various subtypes of a sum type (i.e. of a sealed
   * trait or enum) are combined to a single `Gen`.
   *
   * As a simple (and naive) example, if one would want to always put extra weight on the first subtype, one could do
   * the following:
   * {{{
   * import io.github.martinhh.derived.arbitrary.configured
   *
   * val customDeriving =
   * configured.simple(
   *   [a] =>
   *     (gens: List[Gen[a]]) => {
   *       val frequencies = (2 -> gens.head) +: gens.tail.map(g => 1 -> g)
   *       Gen.frequency(frequencies*)
   *   }
   * )
   * }}}
   *
   * Then, one could do the following to have "fully implicit" derivation in scope:
   * {{{
   * import customDeriving.given
   * }}}
   *
   * Or, for "explicit" derivation:
   * {{{
   * import org.scalacheck.Arbitrary
   *
   * given arbExampleADT: Arbitrary[ExampleADT] = customDeriving.deriveArbitrary
   * }}}
   */
  object configured:

    private sealed trait NoConf[A]

    /**
     * Factory for creating custom `ArbitraryDeriving` without an additional configuration object.
     *
     * @param sumGenFactory The logic for combining the `Gen`-instances of the various subtypes of a sum type.
     * @return An object providing the relevant public API for deriving `Arbitrary`-instances.
     */
    def simple(
      sumGenFactory: [a] => List[Gen[a]] => Gen[a]
    ): ArbitraryDeriving[?] =
      ArbitraryDeriving[
        NoConf
      ]([a] => (l: List[Gen[a]], _: Option[NoConf[a]]) => sumGenFactory[a](l))

    /**
     * Factory for creating custom `ArbitraryDeriving` with support for configuration via `RecursionFallback`.
     *
     * @param sumGenFactory The logic for combining the `Gen`-instances of the various subtypes of a sum type.
     *                      The `Option[RecursionFallback[a]]` will be non-empty if a given instance for
     *                      `RecursionFallback[a]` is in scope where derivation is executed.
     * @return An object providing the relevant public API for deriving `Arbitrary`-instances.
     */
    def recursionFallback(
      sumGenFactory: [a] => (List[Gen[a]], Option[RecursionFallback[a]]) => Gen[a]
    ): ArbitraryDeriving[RecursionFallback] =
      ArbitraryDeriving[RecursionFallback](sumGenFactory)

    /**
     * Factory for creating custom `ArbitraryDeriving` with support for a custom configuration type.
     *
     * This allows to make derivation for sum types depend on a user-provided configuration type.
     *
     * @param sumGenFactory The logic for combining the `Gen`-instances of the various subtypes of a sum type.
     *                      The `Option[SumConfig[a]]` will be non-empty if a given instance for
     *                      `SumConfig[a]` is in scope where derivation is executed.
     * @tparam SumConfig The configuration type.
     * @return An object providing the relevant public API for deriving `Arbitrary`-instances.
     */
    def customConf[SumConfig[_]](
      sumGenFactory: [a] => (List[Gen[a]], Option[SumConfig[a]]) => Gen[a]
    ): ArbitraryDeriving[SumConfig] =
      ArbitraryDeriving[SumConfig](sumGenFactory)

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
