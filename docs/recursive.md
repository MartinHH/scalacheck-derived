# Recursive structures

`scalacheck-derived` can and will derive instances for recursive structures. However: there
are certain aspects of the resulting runtime-behavior that need to be considered.

## Example

For example, consider this `Tree`:

```scala
enum Tree:
  case Node(one: Tree, two: Tree, three: Tree)
  case Leaf(x: Int)
```

The resulting `Gen` for the derived `Arbitrary` would be more or less equivalent to:
```scala
def treeGen: Gen[Tree] =
  Gen.sized { size =>
    if (size <= 0) {
      Gen.fail
    } else {
      Gen.resize(
        size - 1,
        Gen.oneOf(
          Gen.lzy(
            treeGen.flatMap(l => treeGen.flatMap(m => treeGen.flatMap(r => Node(l, m, r))))
          ),
          arbitrary[Int].map(Leaf.apply)
        )
      )
    }
  }
```

Returning `Gen.fail` when the recursion depth exceeds the configured `size` will make scalacheck abort
the generation attempt and then retry (with the next `Seed`). This shall avoid getting stack overflow errors
that could result from unlimited recursion.

However: depending on the data structure, there is a slight risk that a stack overflow even occurs before
the recursion depth exceeds the configured `size`. Furthermore, the failed attempts may lead to undesired
runtime overhead.

## Solution: RecursionFallback

`scalacheck-derived` offers a `RecursionFallback` typeclass that allows to configure a `Gen` that will
be used to (try to) terminate recursion.

For example (based on the `Tree` example from above):

```scala
import io.github.martinhh.derived.arbitrary.deriveArbitrary
import io.github.martinhh.derived.RecursionFallback
import org.scalacheck.Arbitrary

given arbLeaf: Arbitrary[Tree.Leaf] = deriveArbitrary

given RecursionFallback[Tree] = RecursionFallback(arbLeaf.arbitrary)
// alternatively:
// given RecursionFallback[Tree] = RecursionFallback[Tree, Tree.Leaf]

given arbTree: Arbitrary[Tree] = deriveArbitrary
```

Now, the resulting `Gen` for the derived `arbTree` would be more or less equivalent to:
```scala
def treeGen: Gen[Tree] =
  Gen.sized { size =>
    if (size <= 0) {
      // taken from the RecursionFallback
      arbitrary[Int].map(Leaf.apply)
    } else {
      Gen.resize(
        size - 1,
        Gen.oneOf(
          Gen.lzy(
            treeGen.flatMap(l => treeGen.flatMap(m => treeGen.flatMap(r => Node(l, m, r))))
          ),
          arbitrary[Int].map(Leaf.apply)
        )
      )
    }
  }
```

Note that this still does not rule out that depending on the data structure, a stack overflow could even occur before
the recursion depth exceeds the configured `size` (e.g. in case of a tree where each node has a large number of
branches).

## Further mitigations

Alternative/further ways to mitigate this:

### a) Do not use derivation for such structures

An obvious solution is not to use derivation for such structures. Instead, implement a specific solution tailor-made for
your data structure (e.g. using `Gen.sized` and `Gen.resize` with a more specific handling of the size parameter).

### b) Reduce the value of the (scalacheck) size parameter (quick fix)

An alternative option is to reduce the configured generation size parameter. If you do this without providing a 
`RecursionFallback`, this may however lead to a lot of runtime overhead for the growing number of failed attempts.

