package io.github.martinhh.derived.extras.union

import org.scalacheck.Shrink

private type UnionTypedShrinks[A] = UnionTypedTypeClasses[Shrink, A]

@annotation.nowarn("msg=Stream .* is deprecated")
private trait UnionShrinks:

  private def toShrink[A](uts: UnionTypedShrinks[A]): Shrink[A] =
    Shrink { (a: A) =>
      object TheUnapply:
        def unapply(ts: TypedTypeClass[Shrink, ? <: A]): Option[Stream[A]] =
          ts.typeTest.unapply(a).map(ts.instance.shrink(_))
      val streamOpt = uts.instances.collectFirst { case (TheUnapply(stream)) => stream }
      assert(streamOpt.isDefined, "This case should be unreachable")
      streamOpt.get
    }

  transparent inline given unionTypedShrinksMacro[X]: UnionTypedShrinks[X] =
    io.github.martinhh.derived.extras.union.unionTypedShrinksMacro

  transparent inline given shrinkUnion[X](using inline uts: UnionTypedShrinks[X]): Shrink[X] =
    toShrink(uts)
