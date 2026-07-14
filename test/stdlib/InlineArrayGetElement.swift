//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//
//
// RUN: %target-run-simple-swift( \
// RUN:   -enable-experimental-feature ValueGenerics \
// RUN:   -parse-as-library \
// RUN: )
// REQUIRES: executable_test
// REQUIRES: swift_feature_ValueGenerics
// UNSUPPORTED: use_os_stdlib
// END.

import StdlibUnittest

@available(SwiftStdlib 6.5, *)
@main
enum InlineArrayGetElementTests {
  @available(SwiftStdlib 6.5, *)
  static func main() {
    let suite = TestSuite("InlineArrayGetElementTests")

    suite.test("Trivial") {
      let a: InlineArray<4, Int> = [10, 20, 30, 40]
      expectEqual(a.getElement(0), 10)
      expectEqual(a.getElement(1), 20)
      expectEqual(a.getElement(2), 30)
      expectEqual(a.getElement(3), 40)
    }

    suite.test("Reference") {
      let s0 = "zero", s1 = "one", s2 = "two"
      let a: InlineArray<3, String> = [s0, s1, s2]
      expectEqual(a.getElement(0), "zero")
      expectEqual(a.getElement(1), "one")
      expectEqual(a.getElement(2), "two")
    }

    suite.test("Generic") {
      func pick<let N: Int, T: Equatable>(
        _ a: InlineArray<N, T>, at i: Int
      ) -> T {
        return a.getElement(i)
      }
      let a: InlineArray<5, Int> = [1, 2, 3, 4, 5]
      expectEqual(pick(a, at: 0), 1)
      expectEqual(pick(a, at: 4), 5)
    }

    runAllTests()
  }
}
