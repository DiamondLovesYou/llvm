; RUN: opt < %s -nacl-expand-tls -S | FileCheck %s

target datalayout = "p:32:32:32"

@tvar_bss1 = thread_local global i64 0
@tvar_bss2 = thread_local global i32 0


; CHECK-NOT: %tls_struct = type <{ %tls_init_template, %tls_bss_template }>
; CHECK-NOT: %tls_bss_template = type <{ i64, i32, [4 x i8] }>


define i64* @get_tvar_bss1() {
  ret i64* @tvar_bss1
}
; CHECK-LABEL: define i64* @get_tvar_bss1()
; CHECK-NEXT: %tls_raw = call i8* @llvm.nacl.read.tp()
; CHECK-NEXT: %gep_int = ptrtoint i8* %tls_raw to i32
; CHECK-NEXT: %gep = add i32 %gep_int, -16
; CHECK-NEXT: %field = inttoptr i32 %gep to i64*
; CHECK-NEXT: ret i64* %field
