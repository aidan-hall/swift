// RUN: %target-swift-frontend -O -emit-sil -disable-availability-checking -enable-experimental-feature ValueGenerics %s | %FileCheck %s

// REQUIRES: swift_feature_ValueGenerics

// Verify that InlineArray.getElement inlines its body from the stdlib
// wrapper. Because InlineArray is @_addressableForDependencies, self is
// passed by address, so the emitter takes the vector_base_addr +
// index_addr + load projection path.

// CHECK-LABEL: sil @callGetElement
// CHECK: vector_base_addr
// CHECK: index_addr
// CHECK: load
@_silgen_name("callGetElement")
public func call_getElement(_ a: InlineArray<4, Int>, _ i: Int) -> Int {
  return a.getElement(i)
}
