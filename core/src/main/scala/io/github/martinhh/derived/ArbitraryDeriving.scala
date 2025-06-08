package io.github.martinhh.derived

import io.github.martinhh.derived.Gens.productGen
import io.github.martinhh.deriving.*
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

import scala.annotation.implicitNotFound
import scala.compiletime.summonAll
import scala.compiletime.summonFrom
import scala.compiletime.summonInline
import scala.deriving.*

private sealed trait Gens[T]

private case class SumGens[T](singleGens: List[SingleGen[T]]) extends Gens[T]:
  def gens: List[Gen[T]] = singleGens.map(_.gen)

private case class SingleGen[T](tag: Gens.TypeId, gen: Gen[T]) extends Gens[T]

private trait GensSumInstanceSummoner[T, Elem] extends SumInstanceSummoner[T, Elem, Gens]

private object GensSumInstanceSummoner
  extends SumInstanceSummonerCompanion[Gens, GensSumInstanceSummoner]:
  protected def apply[T, Elem](makeGens: => Gens[Elem]): GensSumInstanceSummoner[T, Elem] =
    new GensSumInstanceSummoner[T, Elem]:
      def deriveOrSummonSumInstance: Gens[Elem] = makeGens

  override protected inline def derive[Elem]: Gens[Elem] =
    summonFrom {
      // nested sealed trait - do not use existing givens for those as that would break specific
      // mechanisms for those
      case m: Mirror.SumOf[Elem] =>
        Gens.sumInstance(m)
      // prefer existing givens for product types
      case a: Arbitrary[Elem] =>
        SingleGen(typeNameMacro[Elem], a.arbitrary)
      case m: Mirror.ProductOf[Elem] =>
        SingleGen(typeNameMacro[Elem], productGen(m))
    }

private object Gens:

  type TypeId = String

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
    @implicitNotFound(
      "Derivation failed. No given instance of type Summoner[${E}] was found. This is most likely due to no Arbitrary[${E}] being available"
    )
    type Summoner[E] = GensSumInstanceSummoner[T, E]
    val elems = summonAll[Tuple.Map[s.MirroredElemTypes, Summoner]].toList
      .asInstanceOf[List[Summoner[T]]]
      .map(_.deriveOrSummonSumInstance)
    val combined = elems.foldLeft(List.empty[SingleGen[T]]) {
      case (acc, s: SingleGen[T]) => acc :+ s
      case (acc, s: SumGens[T])   => acc ++ s.singleGens
    }
    SumGens(combined.distinctBy(_.tag))

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
 *
 * @tparam SumConfig Type of an (optional) configuration that allows to configure how the `Gen`-instances of the various
 *                   subtypes of a sum type (i.e. of a sealed trait or enum) are combined to a single `Gen`. See
 *                   `buildSumGen` for its usage.
 */
trait ArbitraryDeriving[SumConfig[_]]:

  /**
   * The logic for combining the `Gen`-instances of the various subtypes of a sum type (i.e. of a sealed trait or enum).
   *
   * This can be overridden to customize that logic.
   *
   * @param gens `Gen`-instances for all subtypes of a sum type.
   * @param config Optional configuration object - if a given instance of `SumConfig[A]` is in implicit scope of
   *               derivation, this will be a non-empty option containing that instance. Otherwise, this will be `None`.
   * @tparam A The "parent" sum type.
   * @return A `Gen` that was build based on the passed `Gen`s.
   */
  protected def buildSumGen[A](gens: List[Gen[A]], config: Option[SumConfig[A]]): Gen[A]

  private inline def sumGen[A](gens: List[Gen[A]]): Gen[A] =
    buildSumGen(gens, configOpt)

  private inline def configOpt[A]: Option[SumConfig[A]] =
    summonFrom {
      case rf: SumConfig[A] => Some(rf)
      case _                => None
    }

  /**
   * Derives an `Arbitrary[T]`, ignoring any `given Arbitrary[T]` that is already in scope.
   *
   * Note that this will ''not'' derive any missing `Arbitrary`-instances for any members of `T` if
   * `T` is a product type (case class or tuple). It will however derive instances for any subtype
   * of `T` if `T` is a sum type (sealed trait or enum) - the restriction for product types will then
   * apply for those subtypes.
   *
   * It can be used to explicitly create given instances:
   * {{{
   *   case class Point(x: Double, y: Double)
   *   given Arbitrary[Point] = deriveArbitraryShallow
   * }}}
   *
   * The following however would fail:
   * {{{
   *   case class Foo(x: Int)
   *   case class Bar(foo: Foo)
   *   // fails with: No given instance of type org.scalacheck.Arbitrary[Foo] was found.
   *   given Arbitrary[Bar] = deriveArbitraryShallow
   * }}}
   */
  final inline def deriveArbitraryShallow[T](using m: Mirror.Of[T]): Arbitrary[T] =
    inline m match
      case s: Mirror.SumOf[T] =>
        // given to support recursion
        given a: Arbitrary[T] = Arbitrary(sumGen(Gens.sumInstance(s).gens))
        a
      case p: Mirror.ProductOf[T] =>
        Arbitrary(Gens.productGen(p))

  /**
   * Derives an `Arbitrary[T]`, ignoring any `given Arbitrary[T]` that is already in scope.
   *
   * Note that this will recursively derive any missing `Arbitrary`-instances for any members/subtypes
   * of `T` (unless such instances are already available in implicit scope).
   *
   * This can be used to explicitly create given instances:
   * {{{
   *   case class Point(x: Double, y: Double)
   *   given Arbitrary[Point] = deriveArbitrary
   * }}}
   */
  @annotation.nowarn("msg=unused") // needed due to https://github.com/lampepfl/dotty/issues/18564
  final inline def deriveArbitrary[T](using m: Mirror.Of[T]): Arbitrary[T] =
    // make derivation available as given (so that dependencies of factories like
    // Arbitrary.arbContainer can be derived):
    import scalacheck.anyGivenArbitrary
    inline m match
      case s: Mirror.SumOf[T] =>
        // given to support recursion (without falling back to the above import
        given a: Arbitrary[T] = Arbitrary(sumGen(Gens.sumInstance(s).gens))
        a
      case p: Mirror.ProductOf[T] =>
        Arbitrary(Gens.productGen(p))

  /**
   * Resolves an `Arbitrary[T]`, using existing given instances or falling back to derivation.
   *
   * Existing given instances (that are in scope) will be preferred over derivation.
   *
   * Importing this will add derivation as fallback to implicit resolution of `Arbitrary`-instances.
   *
   * Note that the following will not work and result in an "Infinite loop in function body":
   * {{{
   *   case class Point(x: Double, y: Double)
   *   given Arbitrary[Point] = anyGivenArbitrary
   * }}}
   * If you intend to derive `Arbitrary`-instances in that way, use `deriveArbitrary` instead.
   */
  final inline given anyGivenArbitrary[T]: Arbitrary[T] =
    summonFrom {
      case a: Arbitrary[T] =>
        a
      case s: Mirror.SumOf[T] =>
        given arb: Arbitrary[T] = Arbitrary(sumGen(Gens.sumInstance(s).gens))
        arb
      case p: Mirror.ProductOf[T] =>
        given arb: Arbitrary[T] = Arbitrary(Gens.productGen(p))
        arb
    }

/**
 * Default implementation of derivation of `Arbitrary`s.
 */
trait DefaultArbitraryDeriving extends ArbitraryDeriving[RecursionFallback]:

  override final protected def buildSumGen[A](
    gens: List[Gen[A]],
    config: Option[RecursionFallback[A]]
  ): Gen[A] =
    Gen.sized { size =>
      if (size <= 0) {
        config.fold(Gen.fail)(_.fallbackGen)
      } else {
        Gen.resize(size - 1, genOneOf(gens))
      }
    }
