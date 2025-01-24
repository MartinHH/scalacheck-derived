package io.github.martinhh.derived.extras

object union extends UnionArbitraries with UnionCogens with UnionShrinks

object literal extends LiteralArbitraries

object all extends LiteralArbitraries with UnionArbitraries with UnionCogens with UnionShrinks
