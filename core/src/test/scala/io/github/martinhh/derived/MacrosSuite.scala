package io.github.martinhh.derived

import io.github.martinhh.*
import org.scalacheck.Shrink

class MacrosSuite extends munit.FunSuite:

  test("isShrinkAnyMacro returns true if there is no other Shrink in scope") {
    assert(isShrinkAnyMacro[SimpleCaseClass])
  }

  test("isShrinkAnyMacro returns false if a given Shrink is defined in local scope") {
    // note: even though the implementation is using Shrink.shrinkAny, we should still get false
    given Shrink[SimpleCaseClass] = Shrink.shrinkAny

    assert(!isShrinkAnyMacro[SimpleCaseClass])
  }

  test("isShrinkAnyMacro returns false if a given Shrink is defined in the companion") {
    assert(!isShrinkAnyMacro[HasGivenInstances])
  }
