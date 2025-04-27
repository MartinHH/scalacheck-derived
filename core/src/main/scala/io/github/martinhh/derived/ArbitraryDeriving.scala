package io.github.martinhh.derived

import io.github.martinhh.deriving.*
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

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
    Gens.derive[Elem](summonInline[Mirror.Of[Elem]])

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
    type Summoner[E] = GensSumInstanceSummoner[T, E]
    val elems = summonAll[Tuple.Map[s.MirroredElemTypes, Summoner]].toList
      .asInstanceOf[List[Summoner[T]]]
      .map(_.deriveOrSummonSumInstance)
    val combined = elems.foldLeft(List.empty[SingleGen[T]]) {
      case (acc, s: SingleGen[T]) => acc :+ s
      case (acc, s: SumGens[T])   => acc ++ s.singleGens
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
private trait ArbitraryDeriving[SumConfig[_]]:

  private inline def sumGen[A](gens: List[Gen[A]]): Gen[A] =
    buildSumGen(gens, configOpt)

  protected def buildSumGen[A](gens: List[Gen[A]], fallbackGen: Option[SumConfig[A]]): Gen[A]

  protected inline def configOpt[A]: Option[SumConfig[A]] =
    summonFrom {
      case rf: SumConfig[A] => Some(rf)
      case _                => None
    }

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
        Arbitrary(sumGen(Gens.sumInstance(s).gens))
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
        given arb: Arbitrary[T] = Arbitrary(sumGen(Gens.sumInstance(s).gens))
        arb
      case p: Mirror.ProductOf[T] =>
        given arb: Arbitrary[T] = Arbitrary(Gens.productGen(p))
        arb
    }

private object ArbitraryDeriving:
  def apply[Conf[_]](
    sumGenFactory: [a] => (List[Gen[a]], Option[Conf[a]]) => Gen[a]
  ): ArbitraryDeriving[Conf] =
    new ArbitraryDeriving[Conf]:
      override protected def buildSumGen[A](
        gens: List[Gen[A]],
        fallbackGen: Option[Conf[A]]
      ): Gen[A] =
        sumGenFactory(gens, fallbackGen)

private trait DefaultArbitraryDeriving extends ArbitraryDeriving[RecursionFallback]:

  override protected def buildSumGen[A](
    gens: List[Gen[A]],
    fallbackGen: Option[RecursionFallback[A]]
  ): Gen[A] =
    Gen.sized { size =>
      if (size <= 0) {
        fallbackGen.fold(Gen.fail)(_.fallbackGen)
      } else {
        Gen.resize(size - 1, genOneOf(gens))
      }
    }
