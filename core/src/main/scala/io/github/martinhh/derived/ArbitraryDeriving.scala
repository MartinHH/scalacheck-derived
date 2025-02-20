package io.github.martinhh.derived

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

import scala.compiletime.erasedValue
import scala.compiletime.summonAll
import scala.compiletime.summonFrom
import scala.compiletime.summonInline
import scala.deriving.*

private sealed trait Gens[T]

private case class SumGens[T](gens: List[SingleGen[T]]) extends Gens[T]:
  def gen: Gen[T] = genOneOf(gens.map(_.gen))

private case class SingleGen[T](tag: Gens.TypeId, gen: Gen[T]) extends Gens[T]

private object Gens:

  type TypeId = String

  // helper typeclass that allows us to use summonAll in derivation for sum-types
  trait SumInstanceSummoner[T, Elem]:
    def deriveOrSummonSumInstance: Gens[Elem]

  object SumInstanceSummoner:
    // factory to avoid "New anonymous class definition will be duplicated at each inline site"
    def apply[T, Elem](makeGens: => Gens[Elem]): SumInstanceSummoner[T, Elem] =
      new SumInstanceSummoner[T, Elem]:
        def deriveOrSummonSumInstance: Gens[Elem] = makeGens

    inline def makeInstance[T, Elem](inline derive: => Gens[Elem]): SumInstanceSummoner[T, Elem] =
      SumInstanceSummoner {
        inline erasedValue[Elem] match
          case _: T =>
            inline erasedValue[T] match
              case _: Elem =>
                endlessRecursionError
              case _ =>
                derive
          case _ =>
            summonInline[Gens[Elem]]
      }

    inline given instance[T, Elem]: SumInstanceSummoner[T, Elem] =
      makeInstance(Gens.derive[Elem](summonInline[Mirror.Of[Elem]]))

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

  inline def productGen[T](p: Mirror.ProductOf[T]): Gen[T] =
    val arbs = summonAll[Tuple.Map[p.MirroredElemTypes, Arbitrary]]
    val genTuple = tupleInstance(arbs.toList.asInstanceOf[List[Arbitrary[?]]], true)
      .asInstanceOf[Gen[p.MirroredElemTypes]]
    genTuple.map(p.fromProduct(_))

  inline def sumInstance[T](s: Mirror.SumOf[T]): SumGens[T] =
    // must be lazy for support of recursive structures
    type Summoner[E] = SumInstanceSummoner[T, E]
    val elems = summonAll[Tuple.Map[s.MirroredElemTypes, Summoner]].toList
      .asInstanceOf[List[Summoner[T]]]
      .map(_.deriveOrSummonSumInstance)
    val combined = elems.foldLeft(List.empty[SingleGen[T]]) {
      case (acc, s: SingleGen[T]) => acc :+ s
      case (acc, s: SumGens[T])   => acc ++ s.gens
    }
    SumGens(combined.distinctBy(_.tag))

  inline def derive[T](m: Mirror.Of[T]): Gens[T] =
    inline m match
      case s: Mirror.SumOf[T] =>
        sumInstance(s)
      case p: Mirror.ProductOf[T] =>
        SingleGen(typeNameMacro[T], productGen(p))

  inline given derived[T]: Gens[T] =
    summonFrom {
      case a: Arbitrary[T] =>
        SingleGen(typeNameMacro[T], a.arbitrary)
      // both cases below are coded out (instead of just delegating to derive) to saves us some
      // nested inlining (so that we do not hit the Xmaxinlines limit as quickly)
      case s: Mirror.SumOf[T] =>
        sumInstance(s)
      case p: Mirror.ProductOf[T] =>
        SingleGen(typeNameMacro[T], productGen(p))
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
        Arbitrary(Gens.productGen(p))

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
      case s: Mirror.SumOf[T] =>
        given arb: Arbitrary[T] = Arbitrary(Gens.sumInstance(s).gen)
        arb
      case p: Mirror.ProductOf[T] =>
        given arb: Arbitrary[T] = Arbitrary(Gens.productGen(p))
        arb
    }
