//===- PackTypeElimination.swift - Eliminate packs in function arguments -===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import AST
import SIL

/// Eliminates pack element archetype (@pack_element) types by recursively
/// cloning open_pack_element instructions and their type-dependent users.
///
/// If the type that a @pack_element type maps to can be statically determined,
/// cloning its corresponding open_pack_element instruction will register a
/// mapping from the @pack_element type to the concrete type. By recursively
/// cloning all type-dependent users of the open_pack_element, we eliminate the
/// @pack_element type. This enables generic specialization of code that uses
/// pack types.
///
/// ```
///   %15 = integer_literal $Builtin.Word, 0
///   %16 = function_ref @generic_op : $@convention(method) <T where T : VectorArithmetic> (Double, @in_guaranteed T) -> @out T
///   %17 = dynamic_pack_index %15 of $Pack{Double, Double}
///   %18 = open_pack_element %17 of <each Value where repeat each Value : VectorArithmetic> at <Pack{Double, Double}>, shape $each Value, id 6
///   %19 = unchecked_addr_cast %7 to $*@pack_element(6) each Value
///   %20 = unchecked_addr_cast %9 to $*@pack_element(6) each Value
///   %21 = alloc_stack $@pack_element(6) each Value
///   copy_addr %20 to [init] %21
///   %23 = apply %16<@pack_element(6) each Value>(%19, %1, %21) : $@convention(method) <T where T : VectorArithmetic> (Double, @in_guaranteed T) -> @out T
///   destroy_addr %21
///   dealloc_stack %21
/// ```
/// ->
/// ```
///   %16 = function_ref @generic_op : $@convention(method) <T where T : VectorArithmetic> (Double, @in_guaranteed T) -> @out T
///   %19 = unchecked_addr_cast %7 to $*Double
///   %20 = unchecked_addr_cast %9 to $*Double
///   %21 = alloc_stack $Double
///   copy_addr %20 to [init] %21
///   %23 = apply %16<Double>(%19, %1, %21) : $@convention(method) <T where T : VectorArithmetic> (Double, @in_guaranteed T) -> @out T
///   destroy_addr %21
///   dealloc_stack %21
/// ```

let packTypeElimination = FunctionPass(name: "pack-type-elimination") {
  (function: Function, context: FunctionPassContext) in

  if !function.hasOwnership { return }

  
}
