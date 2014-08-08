; RUN: opt < %s -expand-struct-regs -S | FileCheck %s
; RUN: opt < %s -nacl-rewrite-atomics -expand-struct-regs -S | FileCheck %s -check-prefix=CLEANUP

; These two instructions should not appear in the output:
; CLEANUP-NOT: extractvalue
; CLEANUP-NOT: insertvalue

%struct = type { i8, i32 }


define void @struct_load(%struct* %p, i8* %out0, i32* %out1) {
  %val = load %struct* %p
  %field0 = extractvalue %struct %val, 0
  %field1 = extractvalue %struct %val, 1
  store i8 %field0, i8* %out0
  store i32 %field1, i32* %out1
  ret void
}
; CHECK-LABEL: define void @struct_load
; CHECK-NEXT: %val.index{{.*}} = getelementptr %struct* %p, i32 0, i32 0
; CHECK-NEXT: %val.index{{.*}} = getelementptr %struct* %p, i32 0, i32 1
; CHECK-NEXT: %val.field{{.*}} = load i8* %val.index{{.*}}, align 1
; CHECK-NEXT: %val.field{{.*}} = load i32* %val.index{{.*}}, align 1
; CHECK-NEXT: store i8 %val.field{{.*}}, i8* %out0
; CHECK-NEXT: store i32 %val.field{{.*}}, i32* %out1


define void @struct_store(%struct* %in_ptr, %struct* %out_ptr) {
  %val = load %struct* %in_ptr
  store %struct %val, %struct* %out_ptr
  ret void
}
; CHECK-LABEL: define void @struct_store
; CHECK-NEXT: %val.index{{.*}} = getelementptr %struct* %in_ptr, i32 0, i32 0
; CHECK-NEXT: %val.index{{.*}} = getelementptr %struct* %in_ptr, i32 0, i32 1
; CHECK-NEXT: %val.field{{.*}} = load i8* %val.index{{.*}}, align 1
; CHECK-NEXT: %val.field{{.*}} = load i32* %val.index{{.*}}, align 1
; CHECK-NEXT: %out_ptr.index{{.*}} = getelementptr %struct* %out_ptr, i32 0, i32 0
; CHECK-NEXT: %out_ptr.index{{.*}} = getelementptr %struct* %out_ptr, i32 0, i32 1
; CHECK-NEXT: store i8 %val.field{{.*}}, i8* %out_ptr.index{{.*}}, align 1
; CHECK-NEXT: store i32 %val.field{{.*}}, i32* %out_ptr.index{{.*}}, align 1


; Ensure that the pass works correctly across basic blocks.
define void @across_basic_block(%struct* %in_ptr, %struct* %out_ptr) {
  %val = load %struct* %in_ptr
  br label %bb
bb:
  store %struct %val, %struct* %out_ptr
  ret void
}
; CHECK-LABEL: define void @across_basic_block
; CHECK: load
; CHECK: load
; CHECK: bb:
; CHECK: store
; CHECK: store


define void @const_struct_store(%struct* %ptr) {
  store %struct { i8 99, i32 1234 }, %struct* %ptr
  ret void
}
; CHECK-LABEL: define void @const_struct_store
; CHECK: store i8 99
; CHECK: store i32 1234


define void @struct_phi_node(%struct* %ptr) {
entry:
  %val = load %struct* %ptr
  br label %bb
bb:
  %phi = phi %struct [ %val, %entry ]
  ret void
}
; CHECK-LABEL: bb:
; CHECK-NEXT: %phi.index{{.*}} = phi i8 [ %val.field{{.*}}, %entry ]
; CHECK-NEXT: %phi.index{{.*}} = phi i32 [ %val.field{{.*}}, %entry ]


define void @struct_phi_node_multiple_entry(i1 %arg, %struct* %ptr) {
entry:
  %val = load %struct* %ptr
  br i1 %arg, label %bb, label %bb
bb:
  %phi = phi %struct [ %val, %entry ], [ %val, %entry ]
  ret void
}
; CHECK-LABEL: bb:
; CHECK-NEXT: %phi.index{{.*}} = phi i8 [ %val.field{{.*}}, %entry ], [ %val.field{{.*}}, %entry ]
; CHECK-NEXT: %phi.index{{.*}} = phi i32 [ %val.field{{.*}}, %entry ], [ %val.field{{.*}}, %entry ]


define void @struct_select_inst(i1 %cond, %struct* %ptr1, %struct* %ptr2) {
  %val1 = load %struct* %ptr1
  %val2 = load %struct* %ptr2
  %select = select i1 %cond, %struct %val1, %struct %val2
  ret void
}
; CHECK-LABEL: define void @struct_select_inst
; CHECK: %select.index{{.*}} = select i1 %cond, i8 %val1.field{{.*}}, i8 %val2.field{{.*}}
; CHECK-NEXT: %select.index{{.*}} = select i1 %cond, i32 %val1.field{{.*}}, i32 %val2.field{{.*}}


define void @insert_and_extract(i8* %out0, i32* %out1) {
  %temp = insertvalue %struct undef, i8 100, 0
  %sval = insertvalue %struct %temp, i32 200, 1
  %field0 = extractvalue %struct %sval, 0
  %field1 = extractvalue %struct %sval, 1
  store i8 %field0, i8* %out0
  store i32 %field1, i32* %out1
  ret void
}
; CHECK-LABEL: define void @insert_and_extract(i8* %out0, i32* %out1) {
; CHECK-NEXT: store i8 100, i8* %out0
; CHECK-NEXT: store i32 200, i32* %out1
; CHECK-NEXT: ret void


define i32 @extract_from_constant() {
  %ev = extractvalue %struct { i8 99, i32 888 }, 1
  ret i32 %ev
}
; CHECK-LABEL: define i32 @extract_from_constant() {
; CHECK-NEXT: ret i32 888


define void @nested() {
  %a1 = alloca i64
  %a2 = alloca i32
  %a3 = alloca { { i32, i64 } }
  %a = insertvalue { i32, i64 } undef, i32 5, 0
  %b = insertvalue { i32, i64 } %a, i64 6, 1
  %c = insertvalue { { i32, i64 } } undef, { i32, i64 } %b, 0
  %d = insertvalue { { { i32, i64 } }, i64 } undef, { { i32, i64 } } %c, 0
  %e = insertvalue { { { i32, i64 } }, i64 } undef, { i32, i64 } %b, 0, 0

  %f = extractvalue { { { i32, i64 } }, i64 } %d, 0, 0, 1
  %g = extractvalue { { { i32, i64 } }, i64 } %e, 0, 0, 0
  %h = extractvalue { { { i32, i64 } }, i64 } %e, 0
  store i64 %f, i64* %a1
  store i32 %g, i32* %a2
  store { { i32, i64 } } %h, { { i32, i64 } }* %a3
  unreachable
}
; CHECK-LABEL: define void @nested()
; CHECK-NEXT:    %a1 = alloca i64
; CHECK-NEXT:    %a2 = alloca i32
; CHECK-NEXT:    %a3 = alloca { { i32, i64 } }
; CHECK-NEXT:    store i64 6, i64* %a1
; CHECK-NEXT:    store i32 5, i32* %a2
; CHECK-NEXT:    %a3.index = getelementptr { { i32, i64 } }* %a3, i32 0, i32 0
; CHECK-NEXT:    %a3.index.index = getelementptr { i32, i64 }* %a3.index, i32 0, i32 0
; CHECK-NEXT:    %a3.index.index1 = getelementptr { i32, i64 }* %a3.index, i32 0, i32 1
; CHECK-NEXT:    store i32 5, i32* %a3.index.index, align 1
; CHECK-NEXT:    store i64 6, i64* %a3.index.index1, align 1

define void @ignore_cmpxchg() {
  %d = cmpxchg i32* null, i32 undef, i32 undef acquire acquire
  %e = extractvalue { i32, i1 } %d, 0
  unreachable
}
; CHECK-LABEL: define void @ignore_cmpxchg()
; CHECK-NEXT:    %d = cmpxchg i32* null, i32 undef, i32 undef acquire acquire
; CHECK-NEXT:    %e = extractvalue { i32, i1 } %d, 0
