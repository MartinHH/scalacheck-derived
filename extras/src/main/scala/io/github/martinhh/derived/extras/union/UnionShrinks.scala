package io.github.martinhh.derived.extras.union

import org.scalacheck.Shrink

import scala.annotation.implicitNotFound

private type TypedShrink[A] = TypedTypeClass[Shrink, A]

private type UnionTypedShrinks[A] = UnionTypeClasses[TypedShrink, A]

private trait UnionShrinks:

  @annotation.nowarn("cat=deprecation")
  private def toShrink[A](uts: UnionTypedShrinks[A]): Shrink[A] =
    Shrink { (a: A) =>
      object TheUnapply:
        def unapply(iw: InstanceWrapper[TypedShrink, ? <: A]): Option[Stream[A]] =
          val ts: TypedShrink[? <: A] = iw.instance
          ts.typeTest.unapply(a).map(ts.instance.shrink(_))
      val streamOpt = uts.instances.collectFirst { case TheUnapply(stream) => stream }
      assert(streamOpt.isDefined, "This case should be unreachable")
      streamOpt.get
    }

  transparent inline given unionTypedShrinksMacro[X]: UnionTypedShrinks[X] =
    io.github.martinhh.derived.extras.union.unionTypedShrinksMacro

  transparent inline given shrinkUnion[X](
    using @implicitNotFound(
      "Could not find a given instance for UnionTypedShrinks[${X}].\n" +
        "Reason might be that ${X} is not a union or (if ${X} is a union)\n" +
        "that there is no given instance for one of its type member"
    ) inline uts: UnionTypedShrinks[X]
  ): Shrink[X] =
    toShrink(uts)
