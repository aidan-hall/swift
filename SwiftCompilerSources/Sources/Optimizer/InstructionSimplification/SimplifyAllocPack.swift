//===--- SimplifyAllocStack.swift -----------------------------------------===//
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

import SIL

/// Eliminate an alloc_pack with a fully concrete indirect pack type. To do this
/// we (currently) require the following:
///
/// - The pack is indirect (pack elements are addresses). This is currently true
///   for (almost) all packs.
///
/// - The pack contains no pack expansion types.
///
/// - The only users of the pack are pack_element_get, pack_element_set,
///   dealloc_pack and debug_value instructions.
///
/// - Every pack_element_{get,set} instruction that accesses the pack uses a
///   scalar_pack_index. This means the accessed pack element is statically
///   known, so we can replace each pack_element_get with the corresponding
///   address that was stored by pack_element_set.
///
/// Before:
///
///   %0 = alloc_pack $Pack{Int, Float}
///   %1 = scalar_pack_index 0 of $Pack{Int, Float}
///   pack_element_set %intptr into %1 of %0
///   %2 = scalar_pack_index 1 of $Pack{Int, Float}
///   pack_element_set %floatptr into %2 of %0
///   ...
///   %intptr2 = pack_element_get %1 of %0
///   use_int_ptr %intptr2
///   %floatptr2 = pack_element_get %2 of %0
///   use_float_ptr %floatptr2
///   dealloc_pack %0
///
/// After:
///
///  %0 = alloc_stack $(Int, Float)
///  %intptr2 = tuple_element_addr %0, 0
///  %floatptr2 = tuple_element_addr %0, 1
///  copy_addr %intptr to [init] %intptr2
///  copy_addr %floatptr to [init] %floatptr2
///  ...
///  use_int_ptr %intptr2
///  use_float_ptr %floatptr2
///  dealloc_stack %0
///   
extension AllocPackInst : Simplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    let packType = self.packType.loweredType(in: self.parentFunction)
    if !packType.isSILPackElementAddress || packType.containsSILPackExpansionType {
      return
    }

    // Collect users.
    var packElementGets: [(index: Int, instruction: PackElementGetInst)] = []
    var packElementSets: [(index: Int, instruction: PackElementSetInst)] = []
    var deallocPacks: [DeallocPackInst] = []
    var debugValues: [DebugValueInst] = []
    var dynamicSet = false
    var dynamicGet = false

    for user in self.users {
      switch user {
      case let peg as PackElementGetInst:
        // Can only eliminate a pack that is accessed with scalar indices.
        if let spi = peg.indexOperand.value as? ScalarPackIndexInst {
          packElementGets.append((index: spi.componentIndex, instruction: peg))
        } else {
          dynamicGet = true
        }

      case let pes as PackElementSetInst:
        // Can only eliminate a pack that is accessed with scalar indices.
        if let spi = pes.indexOperand.value as? ScalarPackIndexInst {
          packElementSets.append((index: spi.componentIndex, instruction: pes))
        } else {
          dynamicSet = true
        }
        
      case let dealloc as DeallocPackInst:
        deallocPacks.append(dealloc)

      case let debugValue as DebugValueInst:
        debugValues.append(debugValue)
        continue

      default:
        // The pack cannot be eliminated if any other type of instruction uses it.
        return
      }
    }

    // If there are no gets, any sets are dead, so we can immediately erase all
    // other instructions.
    if packElementGets.isEmpty && !dynamicGet {
      context.erase(instructionIncludingAllUsers: self)
      return
    }

    // If one of the gets or sets used a non-scalar pack index, we cannot
    // eliminate the pack.
    if dynamicGet || dynamicSet {
      return
    }

    // Create substitute alloc_stack.
    let elementTypes: [Type] = packType.packElements.map { $0 }
    let tupleType = context.getTupleType(elements: elementTypes).loweredType(
      in: self.parentFunction)

    let setupBuilder = Builder(after: self, context)
    let allocTuple = setupBuilder.createAllocStack(tupleType)
    var elementAddrs: [TupleElementAddrInst] = []
    for index in elementTypes.indices {
      elementAddrs.append(setupBuilder.createTupleElementAddr(tupleAddress: allocTuple, elementIndex: index))
    }

    // Insert a dealloc_stack corresponding to each dealloc_pack
    for deallocPack in deallocPacks {
      let deallocBuilder = Builder(before: deallocPack, context)
      deallocBuilder.createDeallocStack(allocTuple)
    }

    // Replace pack_element_set with copy_addr
    for (index, packElementSet) in packElementSets {
      let setBuilder = Builder(after: packElementSet, context)
      setBuilder.createCopyAddr(from: packElementSet.valueOperand.value, to: elementAddrs[index], takeSource: false, initializeDest: true)
    }

    // Replace pack_element_get with tuple_element_addr

    // Erase the alloc_pack and all its associated instructions.
    context.erase(instructionIncludingAllUsers: self)
  }
}

