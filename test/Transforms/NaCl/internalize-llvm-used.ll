; RUN: opt %s -pnacl-abi-simplify-preopt -S | FileCheck %s

; Check that -internalize is internalizing functions in @llvm.used.

@llvm.used = appending global [1 x i8*] [i8* bitcast (void ()* @f to i8*)], section "llvm.metedata"

define void @f() {
; CHECK-LABEL: define internal void @f()
  ret void
}
