package io.github.martinhh.derived.extras

object union extends UnionArbitraries with UnionCogens

object literal extends LiteralArbitraries

object all extends LiteralArbitraries with UnionArbitraries with UnionCogens
