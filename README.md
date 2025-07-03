# scalacheck-derived

[![CI](https://github.com/martinhh/scalacheck-derived/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/MartinHH/scalacheck-derived/actions/workflows/ci.yml?query=branch%3Amain) [![Maven Central](https://maven-badges.sml.io/sonatype-central/io.github.martinhh/scalacheck-derived_3/badge.svg?version=0.10.0)](https://maven-badges.sml.io/sonatype-central/io.github.martinhh/scalacheck-derived_3?version=0.10.0)

Automatic derivation of [scalacheck](https://github.com/typelevel/scalacheck) `Arbitrary` (and `Cogen` and `Shrink`)
instances for Scala 3.

This enables automatic derivation for enums, case classes and sealed traits.

Since version 0.6.0, an additional module also supports automatic derivation for literal and union types - see
[docs/extras.md](docs/extras.md) for more.

This library supports regular scala on jvm, scala-js and scala-native. See
[version matrix](#version-matrix) for details on versions.

## Getting started

Add a (test-)dependency on this project to your project. For example, in a regular (jvm-targeting)
sbt project:

```
libraryDependencies ++= Seq(
  "io.github.martinhh" %% "scalacheck-derived" % "<version>" % "test"
)
```

## Usage

There are four ways of using this library. Let's look at them by examples using the following
(example) data structure:

```
enum Color:
  case Red, Blue, Yellow

sealed trait LibItem
case class Magazine(title: String, issue: Int, color: Color) extends LibItem
case class Book(title: String, author: String, color: Color) extends LibItem
```

### a) fully implicit (fully recursive)

By importing `io.github.martinhh.derived.scalacheck.given`, you enable implicit resolution of
derived instances so that you can do this:

```
import io.github.martinhh.derived.scalacheck.given
import org.scalacheck.Prop

Prop.forAll { (item: LibItem) =>
  // your test using item here...
}
```

This will recursively derive any missing instances (for members of case classes & tuples and for subtypes of sealed
traits & enums).

Note that while this is convenient, this style of "ad-hoc-derivation" comes with the risk of instance being derived
redundantly, potentially leading to overhead in compilation times that could be avoided.

### b) derive explicitly (fully recursive)

Alternatively, you can use `io.github.martinhh.derived.scalacheck.deriveArbitrary` to explicitly
derive instances where you need them:

```
import io.github.martinhh.derived.scalacheck.deriveArbitrary
import org.scalacheck.Arbitrary

given arbLibItem: Arbitrary[LibItem] = deriveArbitrary
```

Just like the "fully implicit" approach, this will recursively derive any missing instances.

### c) derive explicitly ("shallow")

If you'd like more control over what is being derived, you can use 
`io.github.martinhh.derived.scalacheck.deriveArbitraryShallow` to explicitly
derive instances:

```
import io.github.martinhh.derived.scalacheck.deriveArbitraryShallow
import org.scalacheck.Arbitrary

given arbColor: Arbitrary[Color] = deriveArbitraryShallow
given arbLibItem: Arbitrary[LibItem] = deriveArbitraryShallow
```

`deriveArbitraryShallow` will not derive missing instances for members of case classes & tuples. In the above example,
the line `given arbColor: Arbitrary[Color] = deriveArbitraryShallow` is required because without a 
`given Arbitrary[Color]` in scope, derivation of `Arbitrary[LibItem]` would fail.

### c) derive explicitly ("extra-shallow")

If you'd like even more control over what is being derived, you can use
`io.github.martinhh.derived.scalacheck.deriveArbitraryExtraShallow` to explicitly
derive instances:

```
import io.github.martinhh.derived.scalacheck.deriveArbitraryExtraShallow
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

// given instances for each value of color:
given arbColor[C <: Color](using v: ValueOf[C]): Arbitrary[C] = Arbitrary(Gen.const(v.value))

given arbColor: Arbitrary[Color] = deriveArbitraryExtraShallow
given arbMagazine: Arbitrary[Magazine] = deriveArbitraryExtraShallow
given arbBook: Arbitrary[Bool] = deriveArbitraryExtraShallow
given arbLibItem: Arbitrary[LibItem] = deriveArbitraryExtraShallow
```

`deriveArbitraryExtraShallow` will not derive missing instances for members of case classes & tuples nor missing
subtypes of sealed traits or enums, so you have to provide a given instance for each of them explicitly.

### Deriving `Shrink`-instances

Since there is a default/fallback (non-shrinking) `Shrink` instance provided by scalacheck for any type and since
derivation may cause significant compile-time overhead, derivation of `Shrink` instances is optional.

If you feel the need to derive `Shrink` instances, you can do so by either importing
`io.github.martinhh.derived.shrink.given` (for "fully implicit" derivation) or by using
`io.github.martinhh.derived.shrink.deriveShrink`.

## Limitations

There are limitations to the provided mechanism:

### Recursive data structures

While derivation for recursive structures is supported, the runtime-behavior of the derived generators
may be less than ideal and may need further tweaking. See [./docs/recursive.md](./docs/recursive.md) for
details and recommended solutions.

### Maximal number of successive inlines

The derivation mechanism uses inlining. Depending on the size of the data structure for which one
wants to derive an `Arbitrary`, it is possible to hit the compiler's maximum limit for number of
successive inlines.

In that case, compilation will fail with a message like this one:

```
[error]    |                     Maximal number of successive inlines (32) exceeded,
[error]    |                     Maybe this is caused by a recursive inline method?
[error]    |                     You can use -Xmax-inlines to change the limit.
```

#### Workarounds

Since version 0.7.0, the derivation mechanism was optimized to leverage certain changes that came to effect
with Scala 3.4.0 which will drastically reduce the number of recursive inlining,
e.g. allowing derivation for case classes with more than 32 members without raising `-Xmax-inlines`.
You will benefit from these changes if you depend on version 0.7.0 or higher and use Scala 3.4.0 or higher.

If that does not help (or if you are not able to use those versions), there are two ways to handle this:

1. use `-Xmax-inlines` compiler setting to increase the limit
2. instead of deriving an `Arbitrary` for the whole structure at once, derive "intermediate"
   instances for some of its members (and place them into implicit scope).

### Known bugs

Additionally, there may be known bugs - you can check for those [here](https://github.com/MartinHH/scalacheck-derived/issues?q=is%3Aissue%20state%3Aopen%20label%3Abug).

## Version matrix

The following table documents which versions of scalacheck, scala, scala-js and scala-native were
used for each release:

| scalacheck-derived | scalacheck | scala | scala-js | scala-native | java |
|--------------------|------------|-------|----------|--------------|------|
| 0.10.0             | 1.18.1     | 3.3.6 | 1.19.0   | 0.5.8        | 11   |
| 0.9.0              | 1.18.1     | 3.3.6 | 1.19.0   | 0.5.8        | 11   |
| 0.8.2              | 1.18.1     | 3.3.5 | 1.18.2   | 0.5.7        | 11   |
| 0.8.1              | 1.18.1     | 3.3.5 | 1.18.2   | 0.5.7        | 11   |
| 0.8.0              | 1.18.1     | 3.3.5 | 1.18.2   | 0.5.7        | 8    |
| 0.7.1              | 1.18.1     | 3.3.5 | 1.18.2   | 0.5.6        | 8    |
| 0.7.0              | 1.18.1     | 3.3.5 | 1.18.2   | 0.5.6        | 8    |
| 0.6.0              | 1.18.1     | 3.3.5 | 1.18.2   | 0.5.6        | 8    |
| 0.5.0              | 1.18.0     | 3.3.1 | 1.16.0   | 0.5.1        | 8    |
| 0.4.2              | 1.17.0     | 3.3.1 | 1.13.2   | 0.4.15       | 8    |
| 0.4.1              | 1.17.0     | 3.3.0 | 1.13.0   | 0.4.14       | 8    |
| 0.4.0              | 1.17.0     | 3.3.0 | 1.13.0   | 0.4.14       | 17   |
| 0.3.0              | 1.17.0     | 3.3.0 | 1.13.0   | 0.4.14       | 17   |
| 0.2.0              | 1.17.0     | 3.2.2 | 1.13.0   | 0.4.12       | 17   |
| 0.1.0              | 1.17.0     | 3.2.2 | 1.13.0   | 0.4.12       | 17   |
