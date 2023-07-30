package io.github.martinhh.derived

import scala.compiletime.error
import scala.deriving.Mirror

private inline def productToMirroredElemTypes[T](p: Mirror.ProductOf[T])(
  t: T
): p.MirroredElemTypes =
  Tuple.fromProduct(t.asInstanceOf[Product]).asInstanceOf[p.MirroredElemTypes]

// should be impossible to reach (called in case Mirror.SumOf[T].MirroredElemTypes contains T)
private inline def endlessRecursionError: Nothing = error("infinite recursive derivation")
