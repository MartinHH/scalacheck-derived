package io.github.martinhh.deriving

import scala.compiletime.erasedValue
import scala.compiletime.error
import scala.compiletime.summonInline

// helper typeclass that allows us to use summonAll in derivation for sum-types
private[martinhh] trait SumInstanceSummoner[T, Elem, TC[_]]:
  def deriveOrSummonSumInstance: TC[Elem]

// abstract implementation for companions of typeclass-specific SumInstanceSummoner
private[martinhh] trait SumInstanceSummonerCompanion[
  TC[_],
  S[T, Elem] <: SumInstanceSummoner[T, Elem, TC]
]:
  protected def apply[T, Elem](makeTC: => TC[Elem]): S[T, Elem]

  protected inline def derive[Elem]: TC[Elem]

  protected inline def makeInstance[T, Elem](inline derive: => TC[Elem]): S[T, Elem] =
    apply {
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

  inline given sumInstanceSummoner[T, Elem]: S[T, Elem] =
    makeInstance(derive[Elem])
