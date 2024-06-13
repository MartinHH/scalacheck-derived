package io.github.martinhh.derived.extras

import scala.quoted.*

// macro based on this StackOverflow answer by Dmytro Mitin: https://stackoverflow.com/a/78567397/6152669
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

private transparent inline given unionGensMacro[X]: UnionGens[X] =
  ${ unionGens[X] }
