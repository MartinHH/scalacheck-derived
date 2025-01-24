package io.github.martinhh.derived.extras

import org.scalacheck.Cogen
import org.scalacheck.Shrink

import scala.quoted.*

// macros for union based on this StackOverflow answer by Dmytro Mitin:
// https://stackoverflow.com/a/78567397/6152669

private def unionGens[X: Type](using Quotes): Expr[UnionGens[X]] =
  import quotes.reflect.*
  TypeRepr.of[X] match
    case OrType(l, r) =>
      (l.asType, r.asType) match
        case ('[a], '[b]) =>
          (Expr.summon[UnionGens[a]], Expr.summon[UnionGens[b]]) match
            case (Some(aInst), Some(bInst)) =>
              '{
                val x = $aInst
                val y = $bInst
                UnionGens(x.gens ++ y.gens)
              }.asExprOf[UnionGens[X]]
            case (_, _) =>
              report.errorAndAbort(s"Could not summon UnionGens")
    case x =>
      report.errorAndAbort(s"${x.show} is not a union type")

private transparent inline given unionGensMacro[X]: UnionGens[X] =
  ${ unionGens[X] }

private def unionTypedTypeClasses[TC[_]: Type, X: Type](
  using Quotes
): Expr[UnionTypedTypeClasses[TC, X]] =
  import quotes.reflect.*
  TypeRepr.of[X] match
    case OrType(l, r) =>
      (l.asType, r.asType) match
        case ('[a], '[b]) =>
          (Expr.summon[TypedTypeClasses[TC, a]], Expr.summon[TypedTypeClasses[TC, b]]) match
            case (Some(aInst), Some(bInst)) =>
              '{
                val x = $aInst
                val y = $bInst
                UnionTypedTypeClasses[TC, a | b](x.instances ++ y.instances)
              }.asExprOf[UnionTypedTypeClasses[TC, X]]
            case (_, _) =>
              report.errorAndAbort(s"Could not summon instances for Union")
    case x =>
      report.errorAndAbort(s"${x.show} is not a union type")

private transparent inline given unionTypedCogensMacro[X]: UnionTypedCogens[X] =
  ${ unionTypedTypeClasses[Cogen, X] }

private transparent inline given unionTypedShrinksMacro[X]: UnionTypedShrinks[X] =
  ${ unionTypedTypeClasses[Shrink, X] }
