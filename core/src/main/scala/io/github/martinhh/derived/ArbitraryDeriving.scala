package io.github.martinhh.derived

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

import scala.compiletime.erasedValue
import scala.compiletime.summonFrom
import scala.compiletime.summonInline
import scala.deriving.*

private case class Gens[+T](gens: List[Gen[T]]):

  def combine[U >: T](that: Gens[U]): Gens[U] =
    Gens[U](this.gens ++ that.gens)

  def gen: Gen[T] = genOneOf(gens)

private object Gens:

  def apply[T](gen: Gen[T]): Gens[T] = Gens(List(gen))

  // helper for productInstance (runtime-recursion over list with ugly combination of ? and asInstanceOf
  // is a tradeoff with avoiding recursive inlining)
  private def tupleInstance(arbs: List[Arbitrary[?]], isHead: Boolean): Gen[?] =
    // (only) for the very first member of a product, some extra lazyness is needed to
    // ensure we don't end up in an endless loop in case of recursive structures
    def safeGen[A](aArb: Arbitrary[A]): Gen[A] =
      if (isHead) Gen.lzy(aArb.arbitrary) else aArb.arbitrary
    arbs match {
      case xArb :: Nil =>
        safeGen(xArb).map(x => Tuple.apply(x))
      case xArb :: tail =>
        for {
          x <- safeGen(xArb)
          tailTuple <- tupleInstance(tail, false)
        } yield (x *: tailTuple.asInstanceOf[Tuple])
      case Nil =>
        Gen.const(EmptyTuple)
    }

  inline def productInstance[T](p: Mirror.ProductOf[T]): Gens[T] =
    val arbs = scala.compiletime.summonAll[Tuple.Map[p.MirroredElemTypes, Arbitrary]]
    val genTuple = tupleInstance(arbs.toList.asInstanceOf[List[Arbitrary[?]]], true)
      .asInstanceOf[Gen[p.MirroredElemTypes]]
    Gens(genTuple.map(p.fromProduct(_)))

  private inline def summonSumInstances[T, Elems <: Tuple]: List[Gens[T]] =
    inline erasedValue[Elems] match
      case _: (elem *: elems) =>
        deriveOrSummonSumInstance[T, elem].asInstanceOf[Gens[T]] :: summonSumInstances[T, elems]
      case _: EmptyTuple =>
        Nil

  private inline def deriveOrSummonSumInstance[T, Elem]: Gens[Elem] =
    inline erasedValue[Elem] match
      case _: T =>
        inline erasedValue[T] match
          case _: Elem =>
            endlessRecursionError
          case _ =>
            Gens.derive[Elem](summonInline[Mirror.Of[Elem]])
      case _ =>
        summonInline[Gens[Elem]]

  // if we want to reduce depth of inline calls, this could be a candidate for "manual inlining"
  // (but it's called in three places, so significantly more redundancy for one inline less..)
  inline def sumInstance[T](s: Mirror.SumOf[T]): Gens[T] =
    // must be lazy for support of recursive structures
    lazy val elems = summonSumInstances[T, s.MirroredElemTypes]
    elems.reduce(_.combine(_))

  inline def derive[T](m: Mirror.Of[T]): Gens[T] =
    inline m match
      case s: Mirror.SumOf[T] =>
        sumInstance(s)
      case p: Mirror.ProductOf[T] =>
        productInstance(p)

  inline given derived[T]: Gens[T] =
    summonFrom {
      case a: Arbitrary[T] =>
        Gens(a.arbitrary)
      // both cases below are coded out (instead of just delegating to derive) to saves us some
      // nested inlining (so that we do not hit the Xmaxinlines limit as quickly)
      case s: Mirror.SumOf[T] =>
        sumInstance(s)
      case p: Mirror.ProductOf[T] =>
        productInstance(p)
    }

/**
 * Derivation of `Arbitrary`s.
 */
private trait ArbitraryDeriving:

  /**
   * Derives an `Arbitrary[T]`, ignoring any `given Arbitrary[T]` that is already in scope.
   *
   * This can be used to explicitly create given instances:
   * {{{
   *   case class Point(x: Double, y: Double)
   *   given Arbitrary[Point] = deriveArbitrary
   * }}}
   */
  @annotation.nowarn("msg=unused") // needed due to https://github.com/lampepfl/dotty/issues/18564
  inline def deriveArbitrary[T](using m: Mirror.Of[T]): Arbitrary[T] =
    // make derivation available as given (so that dependencies of factories like
    // Arbitrary.arbContainer can be derived):
    import scalacheck.anyGivenArbitrary
    inline m match
      case s: Mirror.SumOf[T] =>
        Arbitrary(Gens.sumInstance(s).gen)
      case p: Mirror.ProductOf[T] =>
        Arbitrary(Gens.productInstance(p).gen)

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
      case a: Arbitrary[T] =>
        a
      case m: Mirror.Of[T] =>
        // this is a given so that the result of derivation is available during derivation (to
        // ensure support for recursive structures - this works because givens are lazy)
        given arb: Arbitrary[T] = Arbitrary(Gens.derive(m).gen)
        arb
    }
