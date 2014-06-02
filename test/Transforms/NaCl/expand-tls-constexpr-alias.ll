; RUN: opt < %s -nacl-expand-tls-constant-expr -S | FileCheck %s

@real_tvar = thread_local global i32 123
@tvar_alias = alias i32* @real_tvar

define i32* @get_tvar() {
  ret i32* @tvar_alias
}
; CHECK: define i32* @get_tvar()
; CHECK: ret i32* @real_tvar


