package io.github.martinhh.derived

import io.github.martinhh.deriving.*
import org.scalacheck.Cogen

import scala.annotation.implicitNotFound
import scala.compiletime.summonAll
import scala.compiletime.summonInline
import scala.compiletime.summonFrom
import scala.deriving.*

trait CogenSumInstanceSummoner[T, Elem] extends SumInstanceSummoner[T, Elem, Cogen]

object CogenSumInstanceSummoner
  extends SumInstanceSummonerCompanion[Cogen, CogenSumInstanceSummoner]:
  protected def apply[T, Elem](makeCogen: => Cogen[Elem]): CogenSumInstanceSummoner[T, Elem] =
    new CogenSumInstanceSummoner[T, Elem]:
      def deriveOrSummonSumInstance: Cogen[Elem] = makeCogen

  override protected inline def derive[Elem]: Cogen[Elem] =
    summonFrom {
      case c: Cogen[Elem] =>
        c
      case m: Mirror.Of[Elem] =>
        cogen.deriveCogen[Elem](using m)
    }

trait CogenDeriving:

  private inline def cogenSum[T](s: Mirror.SumOf[T]): Cogen[T] =
    @implicitNotFound(
      "Derivation failed. No given instance of type Summoner[${E}] was found. This is most likely due to no Cogen[${E}] being available"
    )
    type Summoner[E] = CogenSumInstanceSummoner[T, E]
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

  private inline def cogenProduct[T](p: Mirror.ProductOf[T]): Cogen[T] =
    def cogens = summonAll[Tuple.Map[p.MirroredElemTypes, Cogen]]
    lazy val cogenTuple = tupleInstance(cogens.toList.asInstanceOf[List[Cogen[?]]])
      .asInstanceOf[Cogen[p.MirroredElemTypes]]
    cogenTuple.contramap[T](productToMirroredElemTypes(p)(_))

  @annotation.nowarn("msg=unused") // needed due to https://github.com/lampepfl/dotty/issues/18564
  inline final def deriveCogen[T](using m: Mirror.Of[T]): Cogen[T] =
    import io.github.martinhh.derived.cogen.anyGivenCogen
    given cogen: Cogen[T] = inline m match
      case s: Mirror.SumOf[T]     => cogenSum(s)
      case p: Mirror.ProductOf[T] => cogenProduct(p)
    cogen

  inline final given anyGivenCogen[T]: Cogen[T] =
    summonFrom {
      case c: Cogen[T]        => c
      case s: Mirror.SumOf[T] =>
        given cogen: Cogen[T] = cogenSum(s)
        cogen
      case p: Mirror.ProductOf[T] =>
        given cogen: Cogen[T] = cogenProduct(p)
        cogen
    }
