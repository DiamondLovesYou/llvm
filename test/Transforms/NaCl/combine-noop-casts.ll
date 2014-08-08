; RUN: opt < %s -combine-noop-casts -S | FileCheck %s

target datalayout = "e-m:e-p:32:32-i64:64-v128:64:128-n32-S128"
target triple = "le32-unknown-nacl"

define i64* @ptrtoint_to_bitcast(i32* %ptr) {
  %a = ptrtoint i32* %ptr to i32
  %c = inttoptr i32 %a to i64*
  ret i64* %c
}
; CHECK-LABEL: define i64* @ptrtoint_to_bitcast(i32* %ptr)
; CHECK-NEXT:    %ptr.bc = bitcast i32* %ptr to i64*
; CHECK-NEXT:    ret i64* %ptr.bc

define i32* @ptrtoint_noop(i32* %ptr) {
  %a = ptrtoint i32* %ptr to i32
  %c = inttoptr i32 %a to i32*
  ret i32* %c
}
; CHECK-LABEL: define i32* @ptrtoint_noop(i32* %ptr) {
; CHECK-NEXT:    ret i32* %ptr

define i32 @ptrtoint_untouched(i32* %ptr) {
  %a = ptrtoint i32* %ptr to i32
  ret i32 %a
}
; CHECK-LABEL: define i32 @ptrtoint_untouched(i32* %ptr) {
; CHECK-NEXT:    %a = ptrtoint i32* %ptr to i32

define i32 @no_op_bitcast(i32 %val) {
  %val2 = bitcast i32 %val to i32
  ret i32 %val2
}
; CHECK-LABEL: define i32 @no_op_bitcast(i32 %val) {
; CHECK-NEXT:    ret i32 %val

define i64 @kept_bitcast(double %d) {
  %i = bitcast double %d to i64
  ret i64 %i
}
; CHECK-LABEL: define i64 @kept_bitcast(double %d) {
; CHECK-NEXT:    %i = bitcast double %d to i64

declare void @direct()

define void @expand_constexpr() {
  %.asptr = inttoptr i32 ptrtoint (void ()* @direct to i32) to void ()*
  call void %.asptr()
  ret void
}
; CHECK-LABEL: define void @expand_constexpr()
; CHECK-NEXT:    call void @direct()
; CHECK-NEXT:    ret void
