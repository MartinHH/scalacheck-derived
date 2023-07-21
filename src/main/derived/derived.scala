package io.github.martinhh.derived

import scala.deriving.Mirror

private inline def productToMirroredElemTypes[T](p: Mirror.ProductOf[T])(
  t: T
): p.MirroredElemTypes =
  Tuple.fromProduct(t.asInstanceOf[Product]).asInstanceOf[p.MirroredElemTypes]
