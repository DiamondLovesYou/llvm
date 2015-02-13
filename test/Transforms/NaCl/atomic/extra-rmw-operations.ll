; RUN: opt -nacl-rewrite-atomics -S < %s | FileCheck %s

; Check rewriting nand, max, min, umax, umin atomicrmw operations.

target datalayout = "p:32:32:32"

; We test nand with all types, but for brevity's sake we don't do so for the
; other operations.
define i8 @test_nand_i8(i8* %ptr, i8 %value) {
  %res = atomicrmw nand i8* %ptr, i8 %value seq_cst
  ret i8 %res
}
; CHECK-LABEL: @test_nand_i8
; CHECK:  %1 = load i8* %ptr, align 8
; CHECK:  br label %atomicrmw.start

; CHECK:  %loaded = phi i8 [ %1, %0 ], [ %res, %atomicrmw.start ]
; CHECK:  %2 = and i8 %loaded, %value
; CHECK:  %new = xor i8 %2, -1
; CHECK:  %res = call i8 @llvm.nacl.atomic.rmw.i8(i32 6, i8* %ptr, i8 %new, i32 6)
; CHECK:  %3 = icmp eq i8 %res, %new
; CHECK:  br i1 %3, label %atomicrmw.end, label %atomicrmw.start

; CHECK:  ret i8 %res

define i16 @test_nand_i16(i16* %ptr, i16 %value) {
  %res = atomicrmw nand i16* %ptr, i16 %value seq_cst
  ret i16 %res
}
; CHECK-LABEL: @test_nand_i16
; CHECK:  %1 = load i16* %ptr, align 16
; CHECK:  br label %atomicrmw.start

; CHECK:  %loaded = phi i16 [ %1, %0 ], [ %res, %atomicrmw.start ]
; CHECK:  %2 = and i16 %loaded, %value
; CHECK:  %new = xor i16 %2, -1
; CHECK:  %res = call i16 @llvm.nacl.atomic.rmw.i16(i32 6, i16* %ptr, i16 %new, i32 6)
; CHECK:  %3 = icmp eq i16 %res, %new
; CHECK:  br i1 %3, label %atomicrmw.end, label %atomicrmw.start

; CHECK:  ret i16 %res

define i32 @test_nand_i32(i32* %ptr, i32 %value) {
  %res = atomicrmw nand i32* %ptr, i32 %value seq_cst
  ret i32 %res
}
; CHECK-LABEL: @test_nand_i32
; CHECK:  %1 = load i32* %ptr, align 32
; CHECK:  br label %atomicrmw.start

; CHECK:  %loaded = phi i32 [ %1, %0 ], [ %res, %atomicrmw.start ]
; CHECK:  %2 = and i32 %loaded, %value
; CHECK:  %new = xor i32 %2, -1
; CHECK:  %res = call i32 @llvm.nacl.atomic.rmw.i32(i32 6, i32* %ptr, i32 %new, i32 6)
; CHECK:  %3 = icmp eq i32 %res, %new
; CHECK:  br i1 %3, label %atomicrmw.end, label %atomicrmw.start

; CHECK:  ret i32 %res

define i64 @test_nand_i64(i64* %ptr, i64 %value) {
  %res = atomicrmw nand i64* %ptr, i64 %value seq_cst
  ret i64 %res
}
; CHECK-LABEL: @test_nand_i64
; CHECK:  %1 = load i64* %ptr, align 64
; CHECK:  br label %atomicrmw.start

; CHECK:  %loaded = phi i64 [ %1, %0 ], [ %res, %atomicrmw.start ]
; CHECK:  %2 = and i64 %loaded, %value
; CHECK:  %new = xor i64 %2, -1
; CHECK:  %res = call i64 @llvm.nacl.atomic.rmw.i64(i32 6, i64* %ptr, i64 %new, i32 6)
; CHECK:  %3 = icmp eq i64 %res, %new
; CHECK:  br i1 %3, label %atomicrmw.end, label %atomicrmw.start

; CHECK:  ret i64 %res


define i32 @test_max(i32* %ptr, i32 %value) {
  %res = atomicrmw max i32* %ptr, i32 %value seq_cst
  ret i32 %res
}
; CHECK-LABEL: @test_max
; CHECK:  %1 = load i32* %ptr, align 32
; CHECK:  br label %atomicrmw.start

; CHECK:  %loaded = phi i32 [ %1, %0 ], [ %res, %atomicrmw.start ]
; CHECK:  %2 = icmp sgt i32 %loaded, %value
; CHECK:  %new = select i1 %2, i32 %loaded, i32 %value
; CHECK:  %res = call i32 @llvm.nacl.atomic.rmw.i32(i32 6, i32* %ptr, i32 %new, i32 6)
; CHECK:  %3 = icmp eq i32 %res, %new
; CHECK:  br i1 %3, label %atomicrmw.end, label %atomicrmw.start

; CHECK:  ret i32 %res

define i32 @test_min(i32* %ptr, i32 %value) {
  %res = atomicrmw min i32* %ptr, i32 %value seq_cst
  ret i32 %res
}
; CHECK-LABEL: @test_min
; CHECK:  %1 = load i32* %ptr, align 32
; CHECK:  br label %atomicrmw.start

; CHECK:  %loaded = phi i32 [ %1, %0 ], [ %res, %atomicrmw.start ]
; CHECK:  %2 = icmp sle i32 %loaded, %value
; CHECK:  %new = select i1 %2, i32 %loaded, i32 %value
; CHECK:  %res = call i32 @llvm.nacl.atomic.rmw.i32(i32 6, i32* %ptr, i32 %new, i32 6)
; CHECK:  %3 = icmp eq i32 %res, %new
; CHECK:  br i1 %3, label %atomicrmw.end, label %atomicrmw.start

; CHECK:  ret i32 %res

define i32 @test_umax(i32* %ptr, i32 %value) {
  %res = atomicrmw umax i32* %ptr, i32 %value seq_cst
  ret i32 %res
}
; CHECK-LABEL: @test_umax
; CHECK:  %1 = load i32* %ptr, align 32
; CHECK:  br label %atomicrmw.start

; CHECK:  %loaded = phi i32 [ %1, %0 ], [ %res, %atomicrmw.start ]
; CHECK:  %2 = icmp ugt i32 %loaded, %value
; CHECK:  %new = select i1 %2, i32 %loaded, i32 %value
; CHECK:  %res = call i32 @llvm.nacl.atomic.rmw.i32(i32 6, i32* %ptr, i32 %new, i32 6)
; CHECK:  %3 = icmp eq i32 %res, %new
; CHECK:  br i1 %3, label %atomicrmw.end, label %atomicrmw.start

; CHECK:  ret i32 %res

define i32 @test_umin(i32* %ptr, i32 %value) {
  %res = atomicrmw umin i32* %ptr, i32 %value seq_cst
  ret i32 %res
}
; CHECK-LABEL: @test_umin
; CHECK:  %1 = load i32* %ptr, align 32
; CHECK:  br label %atomicrmw.start

; CHECK:  %loaded = phi i32 [ %1, %0 ], [ %res, %atomicrmw.start ]
; CHECK:  %2 = icmp ule i32 %loaded, %value
; CHECK:  %new = select i1 %2, i32 %loaded, i32 %value
; CHECK:  %res = call i32 @llvm.nacl.atomic.rmw.i32(i32 6, i32* %ptr, i32 %new, i32 6)
; CHECK:  %3 = icmp eq i32 %res, %new
; CHECK:  br i1 %3, label %atomicrmw.end, label %atomicrmw.start

; CHECK:  ret i32 %res
