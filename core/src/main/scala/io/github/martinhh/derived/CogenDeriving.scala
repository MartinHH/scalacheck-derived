package io.github.martinhh.derived

import io.github.martinhh.deriving.*
import org.scalacheck.Cogen

import scala.annotation.implicitNotFound
import scala.compiletime.summonAll
import scala.compiletime.summonInline
import scala.compiletime.summonFrom
import scala.deriving.*

/**
 * Default implementation of `SumInstanceSummoner` for `Cogen`s.
 *
 * Derives missing instances for products.
 */
private trait DefaultCogenSummoner[T, Elem] extends SumInstanceSummoner[T, Elem, Cogen]

private object DefaultCogenSummoner
  extends SumInstanceSummonerCompanion[Cogen, DefaultCogenSummoner]:
  protected def apply[T, Elem](makeCogen: => Cogen[Elem]): DefaultCogenSummoner[T, Elem] =
    new DefaultCogenSummoner[T, Elem]:
      def deriveOrSummonSumInstance: Cogen[Elem] = makeCogen

  override protected inline def derive[Elem]: Cogen[Elem] =
    summonFrom {
      case c: Cogen[Elem] =>
        c
      case s: Mirror.SumOf[Elem] =>
        CogenDeriving.cogenSum[Elem, DefaultCogenSummoner](s)
      case p: Mirror.ProductOf[Elem] =>
        CogenDeriving.cogenProduct(p)
    }

/**
 * "Extra-shallow" implementation of `SumInstanceSummoner` for `Cogen`s.
 *
 * Does not derive missing instances for products.
 */
private trait ExtraShallowCogenSummoner[T, Elem] extends SumInstanceSummoner[T, Elem, Cogen]

private object ExtraShallowCogenSummoner
  extends SumInstanceSummonerCompanion[Cogen, DefaultCogenSummoner]:
  protected def apply[T, Elem](makeCogen: => Cogen[Elem]): DefaultCogenSummoner[T, Elem] =
    new DefaultCogenSummoner[T, Elem]:
      def deriveOrSummonSumInstance: Cogen[Elem] = makeCogen

  override protected inline def derive[Elem]: Cogen[Elem] =
    summonFrom {
      case c: Cogen[Elem] =>
        c
      case s: Mirror.SumOf[Elem] =>
        CogenDeriving.cogenSum[Elem, ExtraShallowCogenSummoner](s)
      // do not derive for product
    }

private[derived] object CogenDeriving:

  inline def cogenSum[T, S[A, B] <: SumInstanceSummoner[A, B, Cogen]](
    s: Mirror.SumOf[T]
  ): Cogen[T] =
    @implicitNotFound(
      "Derivation failed. No given instance of type Summoner[${E}] was found. This is most likely due to no Cogen[${E}] being available"
    )
    type Summoner[E] = S[T, E]
    def elems = summonAll[Tuple.Map[s.MirroredElemTypes, Summoner]].toList
      .asInstanceOf[List[Summoner[T]]]
      .map(_.deriveOrSummonSumInstance)
    lazy val vec = elems.toVector
    Cogen { (seed, t) =>
      val i = s.ordinal(t)
      given Cogen[t.type] = vec(i).asInstanceOf[Cogen[t.type]]
      // adding the ordinal to "perturbing" ensures that each value of an enum
      // (or enum-like sealed trait of case objects) results in a different seed:
      Cogen.perturb[Int](Cogen.perturb[t.type](seed, t), i)
    }

  // helper for productInstance (runtime-recursion over list with ugly combination of ? and asInstanceOf
  // is a tradeoff with avoiding recursive inlining)
  private def tupleInstance(cogens: List[Cogen[?]]): Cogen[? <: Tuple] =
    cogens match {
      case xCogen :: tail =>
        Cogen[Any *: Tuple] { (seed, tpl) =>
          Cogen.perturb(
            Cogen.perturb(seed, tpl.head)(using xCogen.asInstanceOf[Cogen[Any]]),
            tpl.tail
          )(using tupleInstance(tail).asInstanceOf[Cogen[Tuple]])
        }
      case Nil =>
        Cogen.cogenUnit.contramap(_ => ())
    }

  inline def cogenProduct[T](p: Mirror.ProductOf[T]): Cogen[T] =
    def cogens = summonAll[Tuple.Map[p.MirroredElemTypes, Cogen]]
    lazy val cogenTuple = tupleInstance(cogens.toList.asInstanceOf[List[Cogen[?]]])
      .asInstanceOf[Cogen[p.MirroredElemTypes]]
    cogenTuple.contramap[T](productToMirroredElemTypes(p)(_))

private trait CogenDeriving:

  /**
   * Derive a `Cogen[T]`, ignoring any `given Cogen[T]` that is already in scope.
   *
   * Note that this will ''not'' derive any missing `Cogen`-instances for any members of `T` if
   * `T` is a product type (case class or tuple). It will however derive instances for any subtype
   * of `T` if `T` is a sum type (sealed trait or enum) - the restriction for product types will then
   * apply for those subtypes.
   */
  final inline def deriveCogenShallow[T](using m: Mirror.Of[T]): Cogen[T] =
    inline m match
      case s: Mirror.SumOf[T] =>
        // given to support recursion
        given cogen: Cogen[T] = CogenDeriving.cogenSum[T, DefaultCogenSummoner](s)
        cogen
      case p: Mirror.ProductOf[T] =>
        CogenDeriving.cogenProduct(p)

  /**
   * Derives a `Cogen[T]`, ignoring any `given Cogen[T]` that is already in scope.
   *
   * Note that this will ''not'' derive any missing `Cogen`-instances for any members of `T` if
   * `T` is a product type (case class or tuple) nor will it derive instances for any subtype
   * of `T` if `T` is a sum type (sealed trait or enum).
   */
  final inline def deriveCogenExtraShallow[T](using m: Mirror.Of[T]): Cogen[T] =
    inline m match
      case s: Mirror.SumOf[T] =>
        // given to support recursion
        given cogen: Cogen[T] = CogenDeriving.cogenSum[T, ExtraShallowCogenSummoner](s)
        cogen
      case p: Mirror.ProductOf[T] =>
        CogenDeriving.cogenProduct(p)

  /**
   * Derives a `Cogen[T]`, ignoring any `given Cogen[T]` that is already in scope.
   *
   * Note that this will recursively derive any missing `Cogen`-instances for any members/subtypes
   * of `T` (unless such instances are already available in implicit scope).
   */
  @annotation.nowarn("msg=unused") // needed due to https://github.com/lampepfl/dotty/issues/18564
  inline final def deriveCogen[T](using m: Mirror.Of[T]): Cogen[T] =
    import io.github.martinhh.derived.cogen.anyGivenCogen
    given cogen: Cogen[T] = inline m match
      case s: Mirror.SumOf[T]     => CogenDeriving.cogenSum[T, DefaultCogenSummoner](s)
      case p: Mirror.ProductOf[T] => CogenDeriving.cogenProduct(p)
    cogen

  /**
   * Resolves a `Cogen[T]`, using existing given instances or falling back to derivation.
   *
   * Existing given instances (that are in scope) will be preferred over derivation.
   *
   * Importing this will add derivation as fallback to implicit resolution of `Cogen`-instances.
   */
  inline final given anyGivenCogen[T]: Cogen[T] =
    summonFrom {
      case c: Cogen[T]        => c
      case s: Mirror.SumOf[T] =>
        given cogen: Cogen[T] = CogenDeriving.cogenSum[T, DefaultCogenSummoner](s)
        cogen
      case p: Mirror.ProductOf[T] =>
        given cogen: Cogen[T] = CogenDeriving.cogenProduct(p)
        cogen
    }
