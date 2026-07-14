// RUN: %target-swift-frontend -enable-experimental-feature BuiltinModule -parse-stdlib -emit-silgen %s | %FileCheck %s

// REQUIRES: swift_feature_BuiltinModule

import Swift
import Builtin

// CHECK-LABEL: sil {{.*}} @$s34builtin_extract_element_fixedarray0B0
// CHECK:      vector_extract {{%.*}}, {{%.*}}
@_transparent
public func extract(
  _ array: Builtin.FixedArray<10, Builtin.Int32>,
  at index: Builtin.Word
) -> Builtin.Int32 {
  return Builtin.extractElementFixedArray(array, index)
}
