package io.github.martinhh.deriving

import scala.compiletime.erasedValue
import scala.compiletime.error
import scala.compiletime.summonFrom
import scala.compiletime.summonInline

// helper typeclass that allows us to use summonAll in derivation for sum-types
private[martinhh] trait SumInstanceSummoner[T, Elem, TC[_]]:
  def deriveOrSummonSumInstance: TC[Elem]

private[martinhh] object SumInstanceSummoner:

  // factory to avoid "New anonymous class definition will be duplicated at each inline site"
  def apply[T, Elem, TC[_]](makeGens: => TC[Elem]): SumInstanceSummoner[T, Elem, TC] =
    new SumInstanceSummoner[T, Elem, TC]:
      def deriveOrSummonSumInstance: TC[Elem] = makeGens

  inline def makeInstance[T, Elem, TC[_]](
    inline derive: => TC[Elem]
  ): SumInstanceSummoner[T, Elem, TC] =
    SumInstanceSummoner {
      inline erasedValue[Elem] match
        case _: T =>
          inline erasedValue[T] match
            case _: Elem =>
              error("infinite recursive derivation")
            case _ =>
              derive
        case _ =>
          summonInline[TC[Elem]]
    }
