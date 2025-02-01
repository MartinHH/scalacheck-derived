package io.github.martinhh.derived.extras.union

import org.scalacheck.Arbitrary

import scala.quoted.*

// macros for union based on this StackOverflow answer by Dmytro Mitin:
// https://stackoverflow.com/a/78567397/6152669
// (This is not intended to be a legally binding copyright attribution because applying an idea
// that was expressed in less than 15 lines of example code within similar code that only makes
// a few straightforward calls to a standard API is not legally significant.)

private def unionTypedTypeClasses[TC[_]: Type, X: Type](
  using q: Quotes
): Expr[UnionTypeClasses[TC, X]] =
  import q.reflect.*
  TypeRepr.of[X] match
    case OrType(l, r) =>
      (l.asType, r.asType) match
        case ('[a], '[b]) =>
          val exprOpt: Option[Expr[UnionTypeClasses[TC, X]]] =
            for {
              aTTC <- Expr.summon[TypeClasses[TC, a]]
              bTTC <- Expr.summon[TypeClasses[TC, b]]
            } yield '{
              UnionTypeClasses[TC, a | b]($aTTC.instances ++ $bTTC.instances)
            }.asExprOf[UnionTypeClasses[TC, X]]
          exprOpt.getOrElse(report.errorAndAbort(s"Could not summon instances for Union"))
    case x =>
      report.errorAndAbort(s"${x.show} is not a union type")

private transparent inline given unionTypedCogensMacro[X]: UnionTypedCogens[X] =
  ${ unionTypedTypeClasses[TypedCogen, X] }

private transparent inline given unionTypedShrinksMacro[X]: UnionTypedShrinks[X] =
  ${ unionTypedTypeClasses[TypedShrink, X] }

private transparent inline given unionTypedGensMacro[X]: UnionArbs[X] =
  ${ unionTypedTypeClasses[Arbitrary, X] }
