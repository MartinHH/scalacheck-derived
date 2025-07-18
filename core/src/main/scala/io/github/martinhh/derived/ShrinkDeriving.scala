package io.github.martinhh.derived

import io.github.martinhh.deriving.*
import org.scalacheck.Shrink

import scala.annotation.implicitNotFound
import scala.annotation.tailrec
import scala.compiletime.constValue
import scala.compiletime.summonAll
import scala.compiletime.summonInline
import scala.compiletime.summonFrom
import scala.deriving.*

private trait ShrinkSumInstanceSummoner[T, Elem] extends SumInstanceSummoner[T, Elem, Shrink]

private object ShrinkSumInstanceSummoner
  extends SumInstanceSummonerCompanion[Shrink, ShrinkSumInstanceSummoner]:
  protected def apply[T, Elem](makeShrink: => Shrink[Elem]): ShrinkSumInstanceSummoner[T, Elem] =
    new ShrinkSumInstanceSummoner[T, Elem]:
      def deriveOrSummonSumInstance: Shrink[Elem] = makeShrink

  override protected inline def derive[Elem]: Shrink[Elem] =
    summonFrom {
      case s: Shrink[Elem] =>
        s
      case m: Mirror.Of[Elem] =>
        shrink.deriveShrink[Elem](using m)
    }

trait ShrinkDeriving:
  self =>

  private inline def shrinkSum[T](s: Mirror.SumOf[T]): Shrink[T] =
    // note that this will most certainly never come to effect due to `org.scalacheck.Shrink.shrinkAny]`
    // being in scope as fallback for any `Shrink[E]`
    @implicitNotFound(
      "Derivation failed. No given instance of type Summoner[${E}] was found. This is most likely due to no Shrink[${E}] being available"
    )
    type Summoner[E] = ShrinkSumInstanceSummoner[T, E]
    def elems = summonAll[Tuple.Map[s.MirroredElemTypes, Summoner]].toList
      .asInstanceOf[List[Summoner[T]]]
      .map(_.deriveOrSummonSumInstance)
    lazy val vec = elems.toVector
    Shrink { t =>
      val i = s.ordinal(t)
      vec(i).asInstanceOf[Shrink[t.type]].shrink(t)
    }

  private def replaceElemI[T <: Tuple](i: Int, t: T, elemN: Any): T =
    Tuple.fromArray(t.take(i).toArray ++ Array(elemN) ++ t.drop(i + 1).toArray).asInstanceOf[T]

  // helper for shrinkProduct (runtime-recursion over list with ugly combination of Any and asInstanceOf
  // is a tradeoff with avoiding recursive inlining)
  @tailrec
  private def shrinkTuple[T <: Tuple](
    i: Int,
    size: Int,
    t: T,
    acc: LazyList[T],
    shrinks: List[Shrink[Any]]
  ): LazyList[T] =
    if (i >= size || shrinks.isEmpty) {
      acc
    } else {
      val shrinkI = shrinks.head
      val nonEmptyT = t.asInstanceOf[T & NonEmptyTuple]
      val elemI = nonEmptyT(i)
      val newAcc = acc.lazyAppendedAll(
        shrinkI
          .shrink(elemI)
          .map(ei => replaceElemI[T & NonEmptyTuple](i, nonEmptyT, ei))
      )
      shrinkTuple(i + 1, size, t, newAcc, shrinks.tail)
    }

  private inline def shrinkProduct[T](p: Mirror.ProductOf[T]): Shrink[T] =
    val size: Tuple.Size[p.MirroredElemTypes] = constValue
    val shrinks = scala.compiletime.summonAll[Tuple.Map[p.MirroredElemTypes, Shrink]]
    given Shrink[p.MirroredElemTypes] =
      Shrink.withLazyList { t =>
        shrinkTuple(0, size, t, LazyList.empty, shrinks.toList.asInstanceOf[List[Shrink[Any]]])
      }
    Shrink.xmap[p.MirroredElemTypes, T](p.fromTuple(_), productToMirroredElemTypes(p)(_))

  @annotation.nowarn("msg=unused") // needed due to https://github.com/lampepfl/dotty/issues/18564
  inline final def deriveShrink[T](using m: Mirror.Of[T]): Shrink[T] =
    import self.anyGivenShrink
    given shrink: Shrink[T] = inline m match
      case s: Mirror.SumOf[T]     => shrinkSum(s)
      case p: Mirror.ProductOf[T] => shrinkProduct(p)
    shrink

  inline final given anyGivenShrink[T]: Shrink[T] =
    summonFrom {
      case s: Shrink[T] if !isShrinkAnyMacro[T] =>
        s
      // both cases below are coded out (instead of just delegating to deriveShrink) because
      // Shrink-derivation already is the longest (thus critical) path for inlining (and hitting
      // the "Xmaxinline" limit) in the whole library
      case s: Mirror.SumOf[T] =>
        given shrink: Shrink[T] = shrinkSum(s)
        shrink
      case p: Mirror.ProductOf[T] =>
        given shrink: Shrink[T] = shrinkProduct(p)
        shrink
    }
