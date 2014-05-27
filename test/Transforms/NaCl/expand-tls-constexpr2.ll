; RUN: opt < %s -nacl-expand-tls-constant-expr -nacl-expand-tls -S | FileCheck %s

target datalayout = "p:32:32:32"

@tvar = thread_local global i32 0

define i32 @get_tvar() {
  ret i32 ptrtoint (i32* @tvar to i32)
}
; CHECK-LABEL: define i32 @get_tvar()
; CHECK: %tls_raw = call i8* @llvm.nacl.read.tp()
; CHECK: %gep_int = ptrtoint i8* %tls_raw to i32
; CHECK: %gep = add i32 %gep_int, -4
; CHECK: %field = inttoptr i32 %gep to i32*
; CHECK: %expanded = ptrtoint i32* %field to i32
; CHECK: ret i32 %expanded
