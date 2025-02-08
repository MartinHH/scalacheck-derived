package io.github.martinhh.derived

import org.scalacheck.Shrink

import scala.annotation.tailrec
import scala.compiletime.constValue
import scala.compiletime.erasedValue
import scala.compiletime.ops.int.S
import scala.compiletime.summonInline
import scala.compiletime.summonFrom
import scala.deriving.*

@annotation.nowarn("msg=Stream .* is deprecated")
private trait ShrinkDeriving:

  private inline def summonSumInstances[T, Elems <: Tuple]: List[Shrink[T]] =
    inline erasedValue[Elems] match
      case _: (elem *: elems) =>
        deriveOrSummonSumInstance[T, elem].asInstanceOf[Shrink[T]] :: summonSumInstances[T, elems]
      case _: EmptyTuple =>
        Nil

  private inline def deriveOrSummonSumInstance[T, Elem]: Shrink[Elem] =
    inline erasedValue[Elem] match
      case _: T =>
        inline erasedValue[T] match
          case _: Elem =>
            endlessRecursionError
          case _ =>
            deriveShrink[Elem](using summonInline[Mirror.Of[Elem]])
      case _ =>
        summonInline[Shrink[Elem]]

  private inline def shrinkSum[T](s: Mirror.SumOf[T]): Shrink[T] =
    lazy val vec = summonSumInstances[T, s.MirroredElemTypes].toVector
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
    acc: Stream[T],
    shrinks: List[Shrink[Any]]
  ): Stream[T] =
    if (i >= size || shrinks.isEmpty) {
      acc
    } else {
      val shrinkI = shrinks.head
      val nonEmptyT = t.asInstanceOf[T with NonEmptyTuple]
      val elemI = nonEmptyT(i)
      val newAcc = acc.lazyAppendedAll(
        shrinkI
          .shrink(elemI)
          .map(ei => replaceElemI[T with NonEmptyTuple](i, nonEmptyT, ei))
      )
      shrinkTuple(i + 1, size, t, newAcc, shrinks.tail)
    }

  private inline def shrinkProduct[T](p: Mirror.ProductOf[T]): Shrink[T] =
    val size: Tuple.Size[p.MirroredElemTypes] = constValue
    val shrinks = scala.compiletime.summonAll[Tuple.Map[p.MirroredElemTypes, Shrink]]
    given Shrink[p.MirroredElemTypes] =
      Shrink { t =>
        shrinkTuple(0, size, t, Stream.empty, shrinks.toList.asInstanceOf[List[Shrink[Any]]])
      }
    Shrink.xmap[p.MirroredElemTypes, T](p.fromTuple(_), productToMirroredElemTypes(p)(_))

  @annotation.nowarn("msg=unused") // needed due to https://github.com/lampepfl/dotty/issues/18564
  inline def deriveShrink[T](using m: Mirror.Of[T]): Shrink[T] =
    import io.github.martinhh.derived.shrink.anyGivenShrink
    given shrink: Shrink[T] = inline m match
      case s: Mirror.SumOf[T]     => shrinkSum(s)
      case p: Mirror.ProductOf[T] => shrinkProduct(p)
    shrink

  inline given anyGivenShrink[T]: Shrink[T] =
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
