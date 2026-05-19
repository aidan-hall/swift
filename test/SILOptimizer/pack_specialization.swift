// RUN: %target-swift-frontend %s -emit-ir -O -solver-disable-crash-on-valid-salvage | %FileCheck %s
// RUN: not --crash %target-swift-frontend %s -emit-ir -O -solver-enable-crash-on-valid-salvage

// REQUIRES: swift_in_compiler



// When no witness methods are called on pack elements, all pack code can be fully eliminated.
// CHECK: define {{.*}} { i32, ptr, double } @"$s19pack_specialization8copyPack2xsxxQp_txxQp_tRvzlFs5Int32V_SPys5Int16VGSdQP_Tg5Tf8xx_n"(i32 %0, ptr %1, double %2)
// CHECK-NEXT: entry:
// CHECK-NEXT: insertvalue
// CHECK-NEXT: insertvalue
// CHECK-NEXT: insertvalue
// CHECK-NEXT: ret { i32, ptr, double }
@inline(never)
func copyPack<each A>(xs: repeat each A) -> (repeat each A) {
  return (repeat each xs);
}

public func copyPackCaller(two: UnsafePointer<Int16>) -> (Int32, UnsafePointer<Int16>, Double) {
  return copyPack(xs: 1, two, 3.0)
}

// With open_pack_element elimination, witness methods on pack elements resolve
// to concrete types, enabling full devirtualization and optimization.
// CHECK:       define {{.*}} i32 @"$s19pack_specialization11addTogether2xsxxQp_txxQp_tRvzSjRzlFs5Int32V_QP_Tg5Tf8xx_n"(i32 %0)
// CHECK-NEXT:  entry:
// CHECK-NOT:     call {{.*}} @"$ss18AdditiveArithmeticP1poiyxx_xtFZTj"
// CHECK:         call { i32, i1 } @llvm.sadd.with.overflow.i32(i32 %0, i32 %0)
// CHECK:         ret i32
@inline(never)
func addTogether<each A: Numeric>(xs: repeat each A) -> (repeat each A) {
  return (repeat (each xs + each xs))
}

public func addTogetherCaller() -> Int32 {
  return addTogether(xs: 1)
}

// Dependent/member types (e.g. each T.IntegerLiteralType) must also resolve to
// concrete types when the open_pack_element is eliminated.
// CHECK-LABEL: define {{.*}} double @"$s19pack_specialization8addOneTo4argsxxQp_txxQp_tRvzSjRzlFSd_QP_Tg5Tf8xx_n"(double %0) {{.*}} {
// CHECK-NEXT:  entry:
// CHECK-NOT:     call{{.*}}@"$ss18AdditiveArithmeticP1poiyxx_xtFZTj"
// CHECK:         [[RESULT:%[0-9]+]] = fadd double %0, 1.000000e+00
// CHECK:         ret double
// CHECK: }

// CHECK-LABEL: define {{.*}} { i64, double, i8 } @"$s19pack_specialization8addOneTo4argsxxQp_txxQp_tRvzSjRzlFSi_Sds4Int8VQP_Tg5Tf8xx_n"(i64 %0, double %1, i8 %2) {{.*}} {
// CHECK:         @llvm.sadd.with.overflow.i64(i64 %0, i64 1)
// CHECK:         @llvm.sadd.with.overflow.i8(i8 %2, i8 1)
// CHECK:         fadd double %1, 1.000000e+00
// CHECK: }
@inline(never)
func addOneTo<each T: Numeric>(args arg: repeat each T) -> (repeat each T) {
  return (repeat 1 + each arg)
}

public func addOneToCaller(y: Double) -> Double {
  return addOneTo(args: y)
}

public func addOneToMultiCaller(x: Int, y: Double, z: Int8) -> (Int, Double, Int8) {
  return addOneTo(args: x, y, z)
}

// Iterating over multiple packs simultaneously.
// CHECK: define {{.*}} { i64, i64 } @"$s19pack_specialization8zipPacks2xs2zsx_q_txQp_txxQp_q_xQptRvzRv_SzRzq_RhzSzR_r0_lFs5Int32V_s6UInt32VQP_s4Int8V_s5Int16VQPTg5Tf8xxx_n"(i32 %0, i32 %1, i8 %2, i16 %3)
// CHECK-NOT: alloca
// CHECK:   shl nsw i16 %3, 1
// CHECK:   shl nuw i32 %1, 1
// CHECK:   shl nsw i8 %2, 1
// CHECK:   shl nsw i32 %0, 1
// CHECK: }
@inline(never)
func zipPacks<each A: BinaryInteger, each B: BinaryInteger>(xs: repeat each A, zs: repeat each B) -> (repeat (each A, each B)) {
  return (repeat (each xs * 2, each zs * 2))
}

public func zipAddPacksCaller(x: Int32, y: UInt32, a: Int8, b: Int16)
  -> ((Int32, Int8), (UInt32, Int16)) {
  return zipPacks(xs: x, y, zs: a, b)
}

// When applying a generic function to each element of a pack, the callee can be
// specialized if the (variadic generic) caller is specialized.
func numericOp<T: BinaryInteger>(_ x: T) -> T {
  if x <= 1 {
    return x
  } else {
    let (p, q) = numericLoop(x - 1, x - 2)
    return p + q
  }
}

// CHECK: define {{.*}} i64 @"$s19pack_specialization11numericLoopyxxQp_txxQpRvzSzRzlFs5Int32V_ADQP_Tg5Tf8xx_n"(i32 %0, i32 %1) {{.*}} {
// CHECK: [[SP2:%[0-9]+]] = add nsw i32 %0, -2
// CHECK: [[SP1:%[0-9]+]] = add nsw i32 %0, -1
// CHECK: [[SP_RESULT:%[0-9]+]] = tail call {{.*}} @"$s19pack_specialization11numericLoopyxxQp_txxQpRvzSzRzlFs5Int32V_ADQP_Tg5Tf8xx_n"(i32 [[SP1]], i32 [[SP2]])
// CHECK-NEXT: [[SP_RESULT1:%[^ ]+]] = trunc i64 [[SP_RESULT]] to i32
// CHECK-NEXT: [[SP_RESULT2_64:%[^ ]+]] = lshr i64 [[SP_RESULT]], 32
// CHECK-NEXT: [[SP_RESULT2:%[^ ]+]] = trunc nuw i64 [[SP_RESULT2_64]] to i32
// CHECK: tail call { i32, i1 } @llvm.sadd.with.overflow.i32(i32 [[SP_RESULT1]], i32 [[SP_RESULT2]])
// CHECK: }
@inline(never)
func numericLoop<each T: BinaryInteger>(_ xs: repeat each T) -> (repeat each T) {
  return (repeat numericOp(each xs))
}

// CHECK: define {{.*}} i64 @"$s19pack_specialization15callNumericLoops5Int32V_ADtyF"() {{.*}} {
// CHECK: tail call
// CHECK: }
public func callNumericLoop() -> (Int32, Int32) {
  numericLoop(39, 39)
}

// Issue #82000: Combining pointers and parameter packs.

// CHECK: define {{.*}} i32 @"$s19pack_specialization12applyPointer5input2ops5Int32VSPyxGxQp_AFxxQpXEtRvzlF"(ptr noalias readonly captures(none) %0, ptr readonly captures(none) %1, ptr %2, i64 %3, ptr %"each T") {{.*}} {
// CHECK: }
public func applyPointer<each T>(input: repeat UnsafePointer<each T>, op: (repeat each T) -> Int32) -> Int32 {
  op(repeat (each input).pointee)
}

// CHECK:      define {{.*}} i32 @"$s19pack_specialization4tests5Int32VyF"() {{.*}} {
// CHECK-NEXT: entry:
// CHECK-NEXT:   ret i32 54
// CHECK-NEXT: }
public func test() -> Int32 {
    withUnsafePointer(to: 27) {
        applyPointer(input: $0, $0) { ($0 + $1) }
    }
}

// CHECK: define {{.*}} i32 @"$s19pack_specialization11applyDirect5input2ops5Int32VxxQp_AFxxQpXEtRvzlF"(ptr noalias readonly captures(none) %0, ptr readonly captures(none) %1, ptr %2, i64 %3, ptr %"each T") {{.*}} {
// CHECK: }
public func applyDirect<each T>(input: repeat each T, op: (repeat each T) -> Int32) -> Int32 {
  op(repeat (each input))
}

// CHECK:      define {{.*}} i32 @"$s19pack_specialization10testDirects5Int32VyF"() {{.*}} {
// CHECK-NEXT: entry:
// CHECK-NEXT:   ret i32 54
// CHECK-NEXT: }
public func testDirect() -> Int32 {
    applyDirect(input: 27, 27) { ($0 + $1) }
}

// MARK: rdar://164686498 Result Builders

@resultBuilder
public struct ThingBuilderParameterPack {
    @inline(always)
    public static func buildBlock<each Content: Thing>(_ content: repeat each Content) -> TupleThing<(repeat each Content)> {
        TupleThing((repeat each content))
    }
}

// CHECK:      define {{.*}} void @"$s19pack_specialization19bodyParameterPacks2QryF"(ptr noalias writeonly sret(%T19pack_specialization10TupleThingVyAA02MyD0V_AEtG) captures(none) {{.*}} %0) {{.*}} {
// CHECK-NEXT: entry:
// CHECK-NEXT:   [[ALLOCA:%[0-9]+]] = alloca <{ %T19pack_specialization7MyThingV, %T19pack_specialization7MyThingV }>
// CHECK-NEXT:   call {{.*}} @"$s19pack_specialization7MyThingVACycfCTf4d_n"({{.*}} [[ALLOCA]])
// CHECK-NEXT:   [[SECOND_THING:%[^ ]+]] = getelementptr {{.*}} ptr [[ALLOCA]]
// CHECK-NEXT:   call {{.*}} @"$s19pack_specialization7MyThingVACycfCTf4d_n"({{.*}} [[SECOND_THING]])
// CHECK-NEXT:   memcpy{{.*}} %0, {{.*}} [[ALLOCA]]
// CHECK-NEXT:   ret void
// CHECK-NEXT: }
@ThingBuilderParameterPack
public func bodyParameterPacks2() -> some Thing {
    MyThing()
    MyThing()
}

public final class MyClass {}

public protocol Thing {}
public struct MyThing: Thing {
  public var storage: (MyClass, MyClass, MyClass, MyClass, MyClass, MyClass, MyClass, MyClass, MyClass) = (MyClass(), MyClass(), MyClass(), MyClass(), MyClass(), MyClass(), MyClass(), MyClass(), MyClass())
    @inline(never)
    public init() {}
}

public struct TupleThing<T>: Thing {
    public var value: T
    @inline(always)
    public init(_ value: T) { self.value = value }
}
