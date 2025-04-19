# Recursive structures

`scalacheck-derived` can and will derive instances for recursive structures. However: there
is a tiny risk that they will lead to a stack overflow at runtime.

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
the generation attempt and then retry (with the next `Seed`).

However: depending on the data structure, there is a slight risk that a stack overflow even occurs before
the recursion depth exceeds the configured `size`. Furthermore, the failed attempts may lead to undesired
runtime overhead.

## Mitigations

If you run into such problems, there are different ways to mitigate this:

### a) Do not use derivation for such structures (recommended)

The recommended solution is not to use derivation for such structures. Instead, implement a specific solution using
`Gen.sized` and `Gen.resize` for your data structure which terminates the recursion by returning the recursion-ending
type of the data structure (in the example: `Leaf`) when reaching the limit instead of failing.

### b) Reduce the value of the (scalacheck) size parameter (quick fix)

An alternative is to reduce the configured generation size parameter. This may however lead to a
lot of runtime overhead for the growing number of failed attempts.

