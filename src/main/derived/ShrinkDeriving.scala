package io.github.martinhh.derived

import scala.compiletime.constValue
import scala.compiletime.ops.int.S
import scala.compiletime.summonAll
import scala.compiletime.summonInline
import scala.compiletime.summonFrom
import scala.deriving.*
import org.scalacheck.Shrink

@annotation.nowarn("msg=Stream .* is deprecated")
private trait ShrinkDeriving:

  private inline def shrinkSum[T](s: Mirror.SumOf[T]): Shrink[T] =
    val elems = summonAll[Tuple.Map[s.MirroredElemTypes, Shrink]]
    val vec = elems.toList.toVector
    Shrink { t =>
      val i = s.ordinal(t)
      vec(i).asInstanceOf[Shrink[t.type]].shrink(t)
    }

  private inline def replaceElemI[I <: Int, T <: Tuple](t: T, elemN: Tuple.Elem[T, I]): T =
    (t.take(constValue[I]) ++ Tuple1(elemN) ++ t.drop(constValue[S[I]])).asInstanceOf[T]

  private inline def buildShrinkTuple[I <: Int, T <: Tuple](
    size: Tuple.Size[T],
    t: T,
    acc: Stream[T]
  ): Stream[T] = {
    val i: I = constValue[I]
    if (i >= size) {
      acc
    } else {
      val shrinkI = summonInline[Shrink[Tuple.Elem[T, I]]]
      val elemI: Tuple.Elem[T, I] = t.asInstanceOf[NonEmptyTuple](i).asInstanceOf[Tuple.Elem[T, I]]
      val newAcc = acc.lazyAppendedAll(shrinkI.shrink(elemI).map(ei => replaceElemI[I, T](t, ei)))
      buildShrinkTuple[S[I], T](size, t, newAcc)
    }
  }

  private inline def shrinkProduct[T](p: Mirror.ProductOf[T]): Shrink[T] =
    val size: Tuple.Size[p.MirroredElemTypes] = constValue
    given Shrink[p.MirroredElemTypes] =
      Shrink { t =>
        buildShrinkTuple[0, p.MirroredElemTypes](size, t, Stream.empty)
      }
    Shrink.xmap[p.MirroredElemTypes, T](p.fromTuple(_), productToMirroredElemTypes(p)(_))

  inline def deriveShrink[T](using m: Mirror.Of[T]): Shrink[T] =
    inline m match
      case s: Mirror.SumOf[T]     => shrinkSum(s)
      case p: Mirror.ProductOf[T] => shrinkProduct(p)

  inline given anyGivenShrink[T]: Shrink[T] =
    summonFrom {
      case s: Shrink[T] if !isShrinkAnyMacro[T] => s
      case s: Mirror.SumOf[T]                   => shrinkSum(s)
      case p: Mirror.ProductOf[T]               => shrinkProduct(p)
    }
