//===--- IRGenSILPasses.h - The IRGen Prepare SIL Passes --------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

namespace swift {

class SILType;
class GenericEnvironment;

namespace irgen {
} // end namespace irgen

class SILTransform;

namespace irgen {
class IRGenModule;

/// Create a pass to hoist alloc_stack instructions with non-fixed size.
SILTransform *createAllocStackHoisting();
SILTransform *createLoadableByAddress();
SILTransform *createPackMetadataMarkerInserter();

} // end namespace irgen

/// Returns true if \p t is a large loadable type that the native calling
/// convention requires to be passed indirectly.
bool isLargeLoadableType(GenericEnvironment *GenericEnv, SILType t,
                         irgen::IRGenModule &Mod);
} // end namespace swift
