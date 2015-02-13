; RUN: opt %s -pnacl-sjlj-eh -S | FileCheck %s

; This test ensures -pnacl-sjlj-eh looks for internal definitions of __pnacl_eh_stack.

; FileCheck needs at least one CHECK.
; CHECK: @__pnacl_eh_stack = internal thread_local global i8* null

@__pnacl_eh_stack = internal thread_local global i8* null

declare i32 @external_func(i64 %arg)

; A function to force -pnacl-sjlj-eh to check for __pnacl_eh_stack.
define i32 @invoke_test(i64 %arg) {
  %result = invoke i32 @external_func(i64 %arg)
      to label %cont unwind label %lpad
cont:
  ret i32 %result
lpad:
  %lp = landingpad { i8*, i32 } personality i8* null cleanup
  ret i32 999
}
