package io.github.martinhh.derived.extras.literal

import org.scalacheck.Cogen

import scala.annotation.unused

trait LiteralCogens:

  final given cogenLiteral[A](using @unused ev: ValueOf[A]): Cogen[A] = Cogen(_ => 0L)
