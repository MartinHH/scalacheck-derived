# scalacheck-derived-extras

Since version 0.6.0, the additional module `scalacheck-derived-extras` provides support for literal and union types.

You can add it to your sbt project (as test-dependency) as follows:

```
libraryDependencies ++= Seq(
  "io.github.martinhh" %% "scalacheck-derived" % "<version>" % "test"
)
```

## Literal types

By importing `io.github.martinhh.derived.extras.literal.scalacheck.given`, you get `Arbitrary`- and `Cogen`-instances
for any literal / singleton type (i.e. for any type for which an instance of `ValueOf` is available).

No `Shrink` instances are provided because there is no sense in shrinking singleton types.

N.B.: since there is only a single value for each literal singleton type, generating arbitrary values may not seem
helpful at first - however: this feature can be very helpful when such types are combined via unions (like `1 | 2 | 3`).

## Unions

By importing `io.github.martinhh.derived.extras.union.scalacheck.given`, you get `Arbitrary`- and `Cogen`-instances
for any union type `A | B` (if instances for `A` and `B` are in scope).

By importing `io.github.martinhh.derived.extras.union.shrink.given`, you can opt in to getting `Shrink`-instances
for any union type  `A | B` derived from the instances for `A` and `B`.

### Caveat: Type Erasure for Cogen and Shrink

Derived `Cogen`s and `Shrink`s for union types use `scala.reflect.TypeTest` for matching the type members of the union
at runtime. `TypeTest` is subject to type erasure - you will see related warnings if, for example, you derive a
`Shrink[Option[Int] | String]`. You may ignore these as long as the "outer" type does not occur twice in your union,
i.e.: runtime errors should only occur if you derive something like a `Shrink[Option[Int] | Option[String]]`.

## Combined Example

The above enables you to things like this:

```
import io.github.martinhh.derived.extras.literal.scalacheck.given
import io.github.martinhh.derived.extras.union.scalacheck.given
import org.scalacheck.Prop

Prop.forAll { (oneTwoOrThree: 1 | 2 | 3) =>
  // your test using oneTwoOrThree here...
}
```