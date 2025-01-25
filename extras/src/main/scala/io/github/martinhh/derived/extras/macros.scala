package io.github.martinhh.derived.extras

import org.scalacheck.Cogen
import org.scalacheck.Shrink

import scala.quoted.*

// macros for union based on this StackOverflow answer by Dmytro Mitin:
// https://stackoverflow.com/a/78567397/6152669

private def unionGens[X: Type](using q: Quotes): Expr[UnionGens[X]] =
  import q.reflect.*
  TypeRepr.of[X] match
    case OrType(l, r) =>
      (l.asType, r.asType) match
        case ('[a], '[b]) =>
          val exprOpt: Option[Expr[UnionGens[X]]] =
            for {
              aGens <- Expr.summon[UnionGens[a]]
              bGens <- Expr.summon[UnionGens[b]]
            } yield '{
              UnionGens($aGens.gens ++ $bGens.gens)
            }.asExprOf[UnionGens[X]]
          exprOpt.getOrElse(report.errorAndAbort(s"Could not summon UnionGens"))
    case x =>
      report.errorAndAbort(s"${x.show} is not a union type")

private transparent inline given unionGensMacro[X]: UnionGens[X] =
  ${ unionGens[X] }

private def unionTypedTypeClasses[TC[_]: Type, X: Type](
  using q: Quotes
): Expr[UnionTypedTypeClasses[TC, X]] =
  import q.reflect.*
  TypeRepr.of[X] match
    case OrType(l, r) =>
      (l.asType, r.asType) match
        case ('[a], '[b]) =>
          val exprOpt: Option[Expr[UnionTypedTypeClasses[TC, X]]] =
            for {
              aTTC <- Expr.summon[TypedTypeClasses[TC, a]]
              bTTC <- Expr.summon[TypedTypeClasses[TC, b]]
            } yield '{
              UnionTypedTypeClasses[TC, a | b]($aTTC.instances ++ $bTTC.instances)
            }.asExprOf[UnionTypedTypeClasses[TC, X]]
          exprOpt.getOrElse(report.errorAndAbort(s"Could not summon instances for Union"))
    case x =>
      report.errorAndAbort(s"${x.show} is not a union type")

private transparent inline given unionTypedCogensMacro[X]: UnionTypedCogens[X] =
  ${ unionTypedTypeClasses[Cogen, X] }

private transparent inline given unionTypedShrinksMacro[X]: UnionTypedShrinks[X] =
  ${ unionTypedTypeClasses[Shrink, X] }
