; RUN: opt < %s -simplify-allocas -S | FileCheck %s

%struct = type { i32, i32 }

declare void @receive_alloca(%struct* %ptr)

define void @alloca_fixed() {
  %buf = alloca %struct, align 128
  call void @receive_alloca(%struct* %buf)
  ret void
}
; CHECK-LABEL: define void @alloca_fixed() {
; CHECK-NEXT:    %buf = alloca i8, i32 8, align 128
; CHECK-NEXT:    %buf.bc = bitcast i8* %buf to %struct*
; CHECK-NEXT:    call void @receive_alloca(%struct* %buf.bc)

; When the size passed to alloca is a constant, it should be a
; constant in the output too.
define void @alloca_fixed_array() {
  %buf = alloca %struct, i32 100
  call void @receive_alloca(%struct* %buf)
  ret void
}
; CHECK-LABEL: define void @alloca_fixed_array() {
; CHECK-NEXT:    %buf = alloca i8, i32 800, align 8
; CHECK-NEXT:    %buf.bc = bitcast i8* %buf to %struct*
; CHECK-NEXT:    call void @receive_alloca(%struct* %buf.bc)

define void @alloca_variable(i32 %size) {
  %buf = alloca %struct, i32 %size
  call void @receive_alloca(%struct* %buf)
  ret void
}
; CHECK-LABEL: define void @alloca_variable(i32 %size) {
; CHECK-NEXT:    %buf.alloca_mul = mul i32 8, %size
; CHECK-NEXT:    %buf = alloca i8, i32 %buf.alloca_mul
; CHECK-NEXT:    %buf.bc = bitcast i8* %buf to %struct*
; CHECK-NEXT:    call void @receive_alloca(%struct* %buf.bc)

define void @alloca_alignment_i32() {
  %buf = alloca i32
  ret void
}
; CHECK-LABEL: void @alloca_alignment_i32() {
; CHECK-NEXT:    alloca i8, i32 4, align 4

define void @alloca_alignment_double() {
  %buf = alloca double
  ret void
}
; CHECK-LABEL: void @alloca_alignment_double() {
; CHECK-NEXT:    alloca i8, i32 8, align 8

define void @alloca_lower_alignment() {
  %buf = alloca i32, align 1
  ret void
}
; CHECK-LABEL: void @alloca_lower_alignment() {
; CHECK-NEXT:    alloca i8, i32 4, align 1

define void @i64_alloca() {
  %1 = alloca i8, i64 1024
  ret void
}
; CHECK-LABEL: define void @i64_alloca()
; CHECK-NEXT:    %1 = alloca i8, i64 1024
; CHECK-NEXT:    %.bc = bitcast i8* %1 to i8*
; CHECK-NEXT:    ret void
