package io.github.martinhh.derived

import scala.quoted.*

/**
 * Implementation of [[isShrinkAnyMacro]].
 */
private def isShrinkAny[T](using Type[T])(using Quotes): Expr[Boolean] =
  import quotes.reflect.*
  Implicits.search(TypeRepr.of[org.scalacheck.Shrink[T]]) match
    case is: ImplicitSearchSuccess =>
      Expr(is.tree.asExpr.matches('{ org.scalacheck.Shrink.shrinkAny[T] }))
    case _ =>
      Expr(false)

/**
 * Returns true if the only given `Shrink[T]` in scope is `org.scalacheck.Shrink.shrinkAny]`.
 *
 * This is used to ensure that `Shrink`-instances will be derived (instead of using the fallback
 * implementation `shrinkAny`) even though any other given instances in scope should be preferred
 * over derivation.
 */
private inline def isShrinkAnyMacro[T]: Boolean =
  ${ isShrinkAny[T] }
