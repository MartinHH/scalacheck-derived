package io.github.martinhh.derived

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

import scala.compiletime.erasedValue
import scala.compiletime.summonAll
import scala.compiletime.summonFrom
import scala.compiletime.summonInline
import scala.deriving.*

private case class Gens[+T](gens: List[Gen[T]]):

  private def list: List[Gen[T]] = gens

  def combine[U >: T](that: Gens[U]): Gens[U] =
    Gens[U](this.gens ++ that.gens)

  def gen: Gen[T] = gens match
    case List(gen) => gen
    case gens      => Gen.choose(0, gens.size - 1).flatMap(i => gens(i))

private object Gens:

  def apply[T](gen: Gen[T]): Gens[T] = Gens(List(gen))

  inline private def tupleInstance[T <: Tuple]: Gens[T] =
    inline erasedValue[T] match
      case _: EmptyTuple =>
        Gens(Gen.const(EmptyTuple.asInstanceOf[T]))
      case _: (t *: ts) =>
        val gen: Gen[T] =
          for {
            tVal <- summonInline[Arbitrary[t]].arbitrary
            tsVal <- tupleInstance[ts].gen
          } yield (tVal *: tsVal).asInstanceOf[T]
        Gens(gen)

  inline def productInstance[T](p: Mirror.ProductOf[T]): Gens[T] =
    Gens(tupleInstance[p.MirroredElemTypes].gen.map(p.fromProduct(_)))

  inline def sumInstance[T](s: Mirror.SumOf[T]): Gens[T] =
    val elems = summonAll[Tuple.Map[s.MirroredElemTypes, Gens]]
    val elemsList = elems.toList.asInstanceOf[List[Gens[T]]]
    elemsList.reduce(_.combine(_))

  inline given derived[T]: Gens[T] =
    summonFrom {
      case a: Arbitrary[T]        => Gens(a.arbitrary)
      case s: Mirror.SumOf[T]     => sumInstance(s)
      case p: Mirror.ProductOf[T] => productInstance(p)
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
  inline def deriveArbitrary[T](using m: Mirror.Of[T]): Arbitrary[T] =
    inline m match
      case s: Mirror.SumOf[T]     => Arbitrary(Gens.sumInstance(s).gen)
      case p: Mirror.ProductOf[T] => Arbitrary(Gens.productInstance(p).gen)

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
    summonFrom { case o: Gens[T] =>
      Arbitrary(o.gen)
    }
