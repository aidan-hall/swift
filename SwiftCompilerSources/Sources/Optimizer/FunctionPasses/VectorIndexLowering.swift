//===--- VectorIndexLowering.swift -----------------------------------------==//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import AST
import SIL

/// Eliminates value-based vector indexing instructions by lowering them to
/// address-based equivalents, inserting temporary allocations as necessary.
///
/// ```
///   %arr = ... // Builtin.FixedArray value
///   %elem1 = vector_extract %arr, %idx1
///   %elem2 = vector_extract %arr, %idx2
/// ```
/// ->
/// ```
///   %stk = alloc_stack $Builtin.FixedArray<...>
///   store %arr to %stk
///   ...
///   %base = vector_base_addr %stk
///   %elem1 = index_addr %base, %idx1
///   %elem2 = index_addr %base, %idx2
///
let vectorIndexLowering = FunctionPass(name: "vector-index-lowering") {
  (function: Function, context: FunctionPassContext) in
  lowerVectorIndexingInstructions(in: function, context)
}

private func lowerVectorIndexingInstructions(in function: Function, _ context: FunctionPassContext) {
  var indexingInsts: [HashableValue: [Instruction]] = [:]
  for inst in function.instructions {
    switch inst {
    case let extract as VectorExtractInst:
      let key = extract.vector.hashable
      if let insts = indexingInsts[key] {
        indexingInsts.updateValue(insts + [extract], forKey: key)
      } else {
        indexingInsts[key] = [extract]
      }
    default:
      break
    }
  }
  print(indexingInsts)
}
