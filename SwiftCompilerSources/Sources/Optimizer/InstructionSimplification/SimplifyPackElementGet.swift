//===--- SimplifyWitnessMethod.swift --------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import AST
import SIL

extension PackElementGetInst : Simplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    _ = tryReplaceWithStoredAddress(packElementGet: self, context)
  }
}

/// Replaces redundant `pack_element_get` with already available values.
/// Conceptually similar to RedundantLoadElimination, but currently limited to a
/// single Basic Block.
///
/// If the pack is allocated in the same Basic Block, replace this instruction
/// with the last value that was stored at the pack index it is getting.
///
/// For example:
///
/// %1 = alloc_pack $Pack{repeat each T}
/// pack_element_set %value into %index of %1
/// %3 = pack_element_get %index of %1
/// use %3
///
/// Becomes:
/// ...
/// use %value
///
/// A matching pack_element_set instruction must either use the same pack index
/// SSA value (%index), or be known statically to use the same concrete index,
/// to store the address into the pack.
private func tryReplaceWithStoredAddress(packElementGet: PackElementGetInst, _ context: SimplifyContext) -> Bool {
  if let allocPack = packElementGet.packOperand.value as? AllocPackInst,
     packElementGet.parentBlock == allocPack.parentBlock
  {
    let staticIndex = getStaticIndex(inst: packElementGet)
    var iter: Instruction? = packElementGet
    while let succ = iter, let inst = succ.previous, inst != allocPack {

      if let packElementSet = inst as? PackElementSetInst,
         packElementSet.packOperand.value == allocPack
      {
        if packElementSet.indexOperand == packElementGet.indexOperand
             || (staticIndex != nil && staticIndex == getStaticIndex(inst: packElementSet))
        {
          // The type of the replacement value must match
          let replacementValue = packElementSet.valueOperand.value
          if replacementValue.type == packElementGet.type {
            packElementGet.replace(with: packElementSet.valueOperand.value, context)
            return true
          }
          // Address types may have been cast to a pack element type for
          // storage. The pack index is the same so the types must match:
          // insert an unchecked_addr_cast.
          if replacementValue.type.isAddress {
            let builder = Builder(before: packElementGet, context)
            let castedValue = builder.createUncheckedAddrCast(from: replacementValue, to: packElementGet.type)
            packElementGet.replace(with: castedValue, context)
            return true
          }

          // Couldn't replace this get with the last stored value.
          return false
        }
      }

      iter = succ.previous
    }
  }

  return false
}

private func getStaticIndex(inst: PackElementAccessor) -> Int? {
  (inst.indexOperand.value as? AnyPackIndexInst)?.staticIndex
}

private protocol PackElementAccessor {
  var indexOperand: Operand { get }
  var packOperand: Operand { get }
}

extension PackElementGetInst: PackElementAccessor {}
extension PackElementSetInst: PackElementAccessor {}
