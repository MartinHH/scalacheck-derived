package io.github.martinhh.derived

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

import scala.compiletime.erasedValue
import scala.compiletime.summonAll
import scala.compiletime.summonFrom
import scala.compiletime.summonInline
import scala.deriving.*

/**
 * Derivation of `Arbitrary`s.
 */
private trait ArbitraryDeriving:

  /** Helper for deriving `Gen`s for all subclasses of a "sum" (including sub-traits). */
  private trait GensList[+T]:
    def gens: List[Gen[T]]

  private trait LowPriorityGensListGivens {

    /** Fallback for anything that is not a sum. */
    inline given nonSumGensList[T](using a: Arbitrary[T]): GensList[T] =
      new GensList[T]:
        def gens: List[Gen[T]] = List(a.arbitrary)
  }

  private object GensList extends LowPriorityGensListGivens:

    inline given derived[T](using s: Mirror.SumOf[T]): GensList[T] =
      val elems = summonAll[Tuple.Map[s.MirroredElemTypes, GensList]]
      val elemsList = elems.toList.asInstanceOf[List[GensList[T]]]
      val gs: List[Gen[T]] = elemsList.flatMap(_.gens)
      new GensList[T]:
        def gens: List[Gen[T]] = gs

  inline private def arbitrarySum[T](s: Mirror.SumOf[T]): Arbitrary[T] =
    val gens = summonInline[GensList[T]].gens
    Arbitrary(Gen.choose(0, gens.size - 1).flatMap(i => gens(i)))

  inline private def genTuple[T <: Tuple]: Gen[T] =
    inline erasedValue[T] match
      case _: EmptyTuple =>
        Gen.const(EmptyTuple.asInstanceOf[T])
      case _: (t *: ts) =>
        for {
          tVal <- summonInline[Arbitrary[t]].arbitrary
          tsVal <- genTuple[ts]
        } yield (tVal *: tsVal).asInstanceOf[T]

  inline private def arbitraryProduct[T](p: Mirror.ProductOf[T]): Arbitrary[T] =
    Arbitrary(genTuple[p.MirroredElemTypes].map(p.fromProduct(_)))

  /**
   * Derives an `Arbitrary[T]`, ignoring any `given Arbitrary[T]` that is already in scope.
   *
   * This can be used to explicitly create given instances:
   * {{{
   *   case class Point(x: Double, y: Double)
   *   given Arbitrary[Point] = deriveArbitrary
   * }}}
   */
  inline def deriveArbitrary[T](using m: Mirror.Of[T]): Arbitrary[T] =
    inline m match
      case s: Mirror.SumOf[T]     => arbitrarySum(s)
      case p: Mirror.ProductOf[T] => arbitraryProduct(p)

  /**
   * Resolves an `Arbitrary[T]`, using existing given instances or falling back to derivation.
   *
   * Existing given instances (that are in scope) will be preferred over derivation.
   *
   * Importing this will add derivation as fallback to implicit resolution of `Arbitrary`-
   * instances.
   *
   * Note that the following will not work and result in an "Infinite loop in function body":
   * {{{
   *   case class Point(x: Double, y: Double)
   *   given Arbitrary[Point] = anyGivenArbitrary
   * }}}
   * If you intend to derive `Arbitrary`-instances in that way, use `deriveArbitrary` instead.
   */
  inline given anyGivenArbitrary[T]: Arbitrary[T] =
    summonFrom {
      case a: Arbitrary[T]        => a
      case s: Mirror.SumOf[T]     => arbitrarySum(s)
      case p: Mirror.ProductOf[T] => arbitraryProduct(p)
    }
