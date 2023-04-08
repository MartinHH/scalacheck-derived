package io.github.martinhh.derived

import scala.compiletime.erasedValue
import scala.compiletime.summonAll
import scala.compiletime.summonInline
import scala.compiletime.summonFrom
import scala.deriving.*

import org.scalacheck.Cogen

trait CogenDeriving:

  private inline def cogenSum[T](s: Mirror.SumOf[T]): Cogen[T] =
    val elems = summonAll[Tuple.Map[s.MirroredElemTypes, Cogen]]
    val vec = elems.toList.toVector
    Cogen { (seed, t) =>
      val i = s.ordinal(t)
      given Cogen[t.type] = vec(i).asInstanceOf[Cogen[t.type]]
      // adding the ordinal to "perturbing" ensures that each value of an enum
      // (or enum-like sealed trait of case objects) results in a different seed:
      Cogen.perturb[Int](Cogen.perturb[t.type](seed, t), i)
    }

  private inline def cogenTuple[T <: Tuple]: Cogen[T] =
    inline erasedValue[T] match
      case _: (t *: ts) =>
        val cogentT = summonInline[Cogen[t]]
        given Cogen[ts] = cogenTuple[ts]
        Cogen[t *: ts] { (seed, pair) =>
          Cogen.perturb[ts](Cogen.perturb[t](seed, pair.head)(cogentT), pair.tail)
        }.asInstanceOf[Cogen[T]]
      case _: EmptyTuple =>
        Cogen.cogenUnit.contramap[T](_ => ())

  inline private def cogenProduct[T](p: Mirror.ProductOf[T]): Cogen[T] =
    cogenTuple[p.MirroredElemTypes].contramap[T] { t =>
      Tuple.fromProduct(t.asInstanceOf[Product]).asInstanceOf[p.MirroredElemTypes]
    }

  inline def deriveCogen[T](using m: Mirror.Of[T]): Cogen[T] =
    inline m match
      case s: Mirror.SumOf[T]     => cogenSum(s)
      case p: Mirror.ProductOf[T] => cogenProduct(p)

  inline given anyGivenCogen[T]: Cogen[T] =
    summonFrom {
      case c: Cogen[T]            => c
      case s: Mirror.SumOf[T]     => cogenSum(s)
      case p: Mirror.ProductOf[T] => cogenProduct(p)
    }
