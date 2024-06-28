package io.github.martinhh.derived.extras

import scala.quoted.*

// macros for union based on this StackOverflow answer by Dmytro Mitin: https://stackoverflow.com/a/78567397/6152669
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

private def unionTypedCogens[X: Type](using Quotes): Expr[UnionTypedCogens[X]] =
  import quotes.reflect.*
  TypeRepr.of[X] match
    case OrType(l, r) =>
      (l.asType, r.asType) match
        case ('[a], '[b]) =>
          (Expr.summon[TypedCogens[a]], Expr.summon[TypedCogens[b]]) match
            case (Some(aInst), Some(bInst)) =>
              '{
                val x = $aInst
                val y = $bInst
                UnionTypedCogens[X](x.instances ++ y.instances)
              }.asExprOf[UnionTypedCogens[X]]
            case (_, _) =>
              report.errorAndAbort(s"Could not summon TypedCogens")
    case x =>
      report.errorAndAbort(s"${x.show} is not a union type")

private transparent inline given unionTypedCogensMacro[X]: UnionTypedCogens[X] =
  ${ unionTypedCogens[X] }
