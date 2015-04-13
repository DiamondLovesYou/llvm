; RUN: opt -rewrite-llvm-debugtrap-intrinsic -S %s | FileCheck %s

target datalayout = "p:32:32:32"

declare void @llvm.debugtrap()

define void @f() {
  call void @llvm.debugtrap()
}
