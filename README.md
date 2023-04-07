# scalacheck-derived

Automatic derivation of [scalacheck](https://github.com/typelevel/scalacheck) `Arbitrary` instances for Scala 3.

This enables automatic derivation for enums, case classes and sealed traits.

This library supports regular scala on jvm, scala-js and scala-native. See
[version matrix](#version-matrix) for details on versions.

## Getting started

Add a (test-)dependency on this project to your project. For example, in a regular (jvm-targeting)
sbt project:

```
libraryDependencies ++= Seq(
  "io.github.martinhh" %% "scalacheck-derived" % "0.1.0" % "test"
)
```

## Usage

There are two ways of using this library. Let's look at them by examples using the following
(example) data structure:

```
enum Color:
  case Red, Blue, Yellow

sealed trait LibItem
case class Magazine(title: String, issue: Int, color: Color) extends LibItem
case class Book(title: String, author: String, color: Color) extends LibItem
```

### a) fully implicit

By importing `io.github.martinhh.derived.scalacheck.given`, you enable implicit resolution of
derived instances so that you can do this:

```
import io.github.martinhh.derived.scalacheck.given
import org.scalacheck.Prop

Prop.forAll { (item: LibItem) =>
  // your test using item here...
}
```

### b) derive explicitly

Alternatively, you can use `io.github.martinhh.derived.scalacheck.deriveArbitrary` to explicitly
derive instances where you need them:

```
import io.github.martinhh.derived.scalacheck.deriveArbitrary
import org.scalacheck.Arbitrary

given arbLibItem: Arbitrary[LibItem] = deriveArbitrary
```

## Limitations

There are a few limitations to the provided mechanism:

### Recursive structures

Recursive structures are not supported. Trying derivation with the "fully implicit" approach will
lead to a compiler error. Trying derivation with the "derive explicit" approach may even lead to
an endless loop during generation at runtime(!).

### Maximal number of successive inlines

The derivation mechanism uses inlining. Depending on the size of the data structure for which one
wants to derive an `Arbitrary`, it is possible to hit the compiler's maximum limit for number of
successive inlines.

In that case, compilation *should* fail with a message saying:

```
[error]    |                     Maximal number of successive inlines (32) exceeded,
[error]    |                     Maybe this is caused by a recursive inline method?
[error]    |                     You can use -Xmax-inlines to change the limit.
```

However, it may also fail with a less helpful message like this one (where `MyTestSuite` is the
name of the class in from which derivation was tried from):

```
[error]     |object GensList cannot be accessed as a member of (ArbitraryDeriving_this : 
[error]     |  (ArbitraryDeriving_this² : io.github.martinhh.derived.ArbitraryDeriving)
[error]     |) from class MyTestSuite.
[error]     |
[error]     |where:    ArbitraryDeriving_this  is a value in the initializer of value gens
[error]     |          ArbitraryDeriving_this² is a value in an anonymous function locally defined in class MyTestSuite
```

#### Workarounds

There are two ways to handle this:

1. use `-Xmax-inlines` compile setting to increase the limit
2. instead of deriving an `Arbitrary` for the whole structure at once, derive "intermediate"
   instances for some of its members (and place them into implicit scope).

### Cogens / deriving functions

Derivation of `Cogen` instances is not supported (yet).

This means that you cannot, for example, derive a function of type `LibItem => Int`.

## Version matrix

The following table documents which versions of scalacheck, scala, scala-js and scala-native were
used for each release:

| scalacheck-derived | scalacheck | scala | scala-js | scala-native | 
|--------------------|------------|-------|----------|--------------|
| 0.1.0              | 1.17.0     | 3.2.2 | 1.13.0   | 0.4.12       |
