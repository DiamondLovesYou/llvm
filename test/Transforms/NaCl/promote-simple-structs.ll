; RUN: opt -promote-simple-structs -S %s | FileCheck %s

%not_promoted = type { i32, i64, i32* }
%nested1 = type { %not_promoted }
%nested2 = type { %nested1 }
%promoted1 = type { i32* }
%promoted2 = type { i64  }
%promoted3 = type { %not_promoted (%promoted1)*, i64 (%promoted2*)* }
%promoted4 = type { %promoted1, %promoted3 }
%linked_list = type { %promoted1, %linked_list* }
; CHECK: %"enum.trie::Child<()>.4" = type { i8, [3 x i8], i32 }
%"enum.trie::Child<()>" = type { i8, [3 x i8], [1 x i32] }
; C;HECK: %"struct.trie::TrieNode<()>.2" = type { i32, [16 x %"enum.trie::Child<()>.3"] }
%"struct.trie::TrieNode<()>" = type { i32, [16 x %"enum.trie::Child<()>"] }
; C;HECK: %tydesc = type { i32, i32, void ({}*, i8*)*, void ({}*, i8*)*, void ({}*, i8*)*, void ({}*, i8*)*, i32, { i8*, i32 } }
%tydesc = type { i32, i32, void ({}*, i8*)*, void ({}*, i8*)*, void ({}*, i8*)*, void ({}*, i8*)*, i32, { i8*, i32 } }
%"enum.option::Option<(*libc::types::common::c95::c_void,~local_data::LocalData:Send,local_data::LoanState)>" = type { i8, [3 x i8], [4 x i32] }
%"enum.libc::types::common::c95::c_void" = type {}

; CHECK: %"struct.fmt::parse::SelectArm.468.3538.6608.13362.15204.16580.9" = type { %str_slice.1.3071.6141.12895.14737.16578.10, %7* }

@g1 = global i32 42, align 4

; CHECK-LABEL: @a1 = alias i32* (i32*)* @f1
@a1 = alias %promoted1 (i32*)* @f1

; CHECK-LABEL: define %not_promoted.0 @not_promoted_fun1(%not_promoted.0* %a1, i32 %a2)
define %not_promoted @not_promoted_fun1(%not_promoted* %a1, i32 %a2) {
; CHECK: %1 = load %not_promoted.0* %a1
       %1 = load %not_promoted* %a1
; CHECK: %2 = extractvalue %not_promoted.0 %1, 0
       %2 = extractvalue %not_promoted %1, 0
; CHECK: %3 = add i32 %2, %a2
       %3 = add i32 %2, %a2
; CHECK: %4 = insertvalue %not_promoted.0 %1, i32 %3, 0
       %4 = insertvalue %not_promoted %1, i32 %3, 0
; CHECK: ret %not_promoted.0 %4
       ret %not_promoted %4
}

; CHECK-LABEL: define i32* @f1(i32* %a1)
define %promoted1 @f1(i32* %a1) {
; CHECK: ret i32* %a1
       %1 = insertvalue %promoted1 undef, i32* %a1, 0
       ret %promoted1 %1
}
; CHECK-LABEL: define i32* @f2(i32* %a1)
define i32* @f2(%promoted1 %a1) {
       %1 = extractvalue %promoted1 %a1, 0
; CHECK: ret i32* %a1
       ret i32* %1
}

; CHECK-LABEL: define i64 @f3(i64* %a1)
define i64 @f3(%promoted2* %a1) {
; CHECK: %1 = load i64* %a1
       %1 = load %promoted2* %a1
; CHECK-NOT: %2 = extractvalue %promoted2 %1, 0
       %2 = extractvalue %promoted2 %1, 0
; CHECK: ret i64 %1
       ret i64 %2
}

; CHECK-LABEL: define i64 @f4(i64** %a1)
define i64 @f4(%promoted2** %a1) {
; CHECK: %1 = load i64** %a1
       %1 = load %promoted2** %a1
; CHECK: %2 = load i64* %1
       %2 = load %promoted2*  %1
; CHECK-NOT: %3 = extractvalue %promoted2 %2, 0
       %3 = extractvalue %promoted2 %2, 0
; CHECK: ret i64 %2
       ret i64 %3
}

; CHECK-LABEL: define i32* @f5()
define %promoted1 @f5() {
       %1 = insertvalue %promoted1 undef, i32* @g1, 0
; CHECK: ret i32* @g1
       ret %promoted1 %1
}
; CHECK-LABEL: define %not_promoted.0 @f6(i32* %a1, i64 %a2)
define %not_promoted @f6(%promoted1 %a1, %promoted2 %a2) {
; CHECK: %1 = call i32* @f2(i32* %a1)
       %1 = call i32* @f2(%promoted1 %a1)
; CHECK: %2 = insertvalue %not_promoted.0 undef, i32* %1, 2
       %2 = insertvalue %not_promoted undef, i32* %1, 2
; CHECK: %3 = alloca i64
       %3 = alloca %promoted2
; CHECK: store i64 %a2, i64* %3
       store %promoted2 %a2, %promoted2* %3
; CHECK: %4 = call i64 @f3(i64* %3)
       %4 = call i64 @f3(%promoted2* %3)
; CHECK: %5 = insertvalue %not_promoted.0 %2, i64 %4, 1
       %5 = insertvalue %not_promoted %2, i64 %4, 1
; CHECK: %6 = insertvalue %not_promoted.0 %5, i32 10, 0
       %6 = insertvalue %not_promoted %5, i32 10, 0
; CHECK: ret %not_promoted.0 %6
       ret %not_promoted %6
}
; CHECK-LABEL: define %not_promoted.0 @f7(i32* %a1)
define %not_promoted @f7(%promoted1 %a1) {
Entry:
; CHECK: %0 = call i32* @f2(i32* %a1)
        %0 = call i32* @f2(%promoted1 %a1)
        %1 = load i32* %0
        %2 = icmp eq i32 %1, 0
        br i1 %2, label %Null, label %NotNull

Null:
; CHECK: %3 = call i32* @f1(i32* @g1)
        %3 = call %promoted1 @f1(i32* @g1)
; CHECK: %tmp = call i32* @a1(i32* @g1)
        %tmp = call %promoted1 @a1(i32* @g1)
; CHECK: %4 = call %not_promoted.0 @f7(i32* %3)
        %4 = call %not_promoted @f7(%promoted1 %3)
        br label %Exit

NotNull:
        %5 = phi i32* [ %0, %Entry ]
; CHECK: %6 = call i32* @f1(i32* %5)
        %6 = call %promoted1 @f1(i32* %5)
; CHECK-NOT: %7 = insertvalue %promoted1 undef, i32* %5, 0
        %7 = insertvalue %promoted1 undef, i32* %5, 0
; CHECK-NOT: %8 = insertvalue %promoted2 undef, i64 16, 0
        %8 = insertvalue %promoted2 undef, i64 16, 0
; CHECK: %7 = call %not_promoted.0 @f6(i32* %5, i64 16)
        %9 = call %not_promoted @f6(%promoted1 %7, %promoted2 %8)
        br label %Exit

Exit:
        %10 = phi %not_promoted [ %4, %Null ], [ %9, %NotNull ]
; CHECK: %9 = phi i32* [ %3, %Null ], [ %6, %NotNull ]
        %11 = phi %promoted1 [ %3, %Null ], [ %6, %NotNull ]
; CHECK: %10 = call i32* @f2(i32* %9)
        %12 = call i32* @f2(%promoted1 %11)
        %13 = load i32* %12
        %14 = insertvalue %not_promoted %10, i32 %13, 0
        ret %not_promoted %14
}
; CHECK-LABEL: define %not_promoted.0 @f8(%not_promoted.0 (i32*)* %a1)
define %not_promoted @f8(%not_promoted (%promoted1)* %a1) {
       %1 = alloca i32
       store i32 42, i32* %1
; CHECK: %2 = call i32* @f1(i32* %1)
       %2 = call %promoted1 @f1(i32* %1)
; CHECK: %tmp = call i32* @a1(i32* %1)
       %tmp = call %promoted1 @a1(i32* %1)
; CHECK: %3 = call %not_promoted.0 %a1(i32* %2)
       %3 = call %not_promoted %a1(%promoted1 %2)
       ret %not_promoted %3
}
; CHECK-LABEL: define %not_promoted.0 @f9()
define %not_promoted @f9() {
; CHECK: %1 = call %not_promoted.0 @f8(%not_promoted.0 (i32*)* @f7)
       %1 = call %not_promoted @f8(%not_promoted (%promoted1)* @f7)
       ret %not_promoted %1
}
; CHECK-LABEL: define %promoted3.1 @f10()
define %promoted3 @f10() {
; CHECK: %1 = insertvalue %promoted3.1 undef, %not_promoted.0 (i32*)* @f7, 0
       %1 = insertvalue %promoted3 undef, %not_promoted (%promoted1)* @f7, 0
; CHECK: %2 = insertvalue %promoted3.1 %1, i64 (i64*)* @f3, 1
       %2 = insertvalue %promoted3 %1, i64 (%promoted2*)* @f3, 1
; CHECK: ret %promoted3.1 %2
       ret %promoted3 %2
}
; CHECK-LABEL: define %not_promoted.0 @f11()
define %not_promoted @f11() {
; CHECK: %1 = call %promoted3.1 @f10()
       %1 = call %promoted3 @f10()
; CHECK: %2 = extractvalue %promoted3.1 %1, 0
       %2 = extractvalue %promoted3 %1, 0
; CHECK: %3 = extractvalue %promoted3.1 %1, 1
       %3 = extractvalue %promoted3 %1, 1
; CHECK: %4 = call %not_promoted.0 @f8(%not_promoted.0 (i32*)* %2)
       %4 = call %not_promoted @f8(%not_promoted (%promoted1)* %2)
; CHECK: ret %not_promoted.0 %4
       ret %not_promoted %4
}
define %promoted1 @f12() {
       %1 = bitcast %promoted3 ()* @f10 to i32*
       %2 = call %promoted1 @f1(i32* %1)
       %3 = call %not_promoted @f7(%promoted1 %2)
       ret %promoted1 %2
}
define void @f13() {
       %1 = call %promoted1 @f12()
       %2 = call i32* @f2(%promoted1 %1)
       %3 = bitcast i32* %2 to %promoted3 ()*
       %4 = call %promoted3 %3()
       ret void
}
define void @f14(%linked_list* %a1, %linked_list* %a2) {
       %1 = load %linked_list* %a1
       %2 = insertvalue %linked_list %1, %linked_list* %a2, 1
       store %linked_list %2, %linked_list* %a1
       ret void
}

; Function Attrs: inlinehint uwtable
define internal void @_ZN4trie8TrieNode3new69h3210031e2fef4109c0163f68203dffa433c4a299f6bad390a11a2f2c49d0df2cJyaj8v0.9.preE(%"struct.trie::TrieNode<()>"* noalias sret, { i32, %tydesc*, i8*, i8*, i8 }*) unnamed_addr #4 {
"function top level":
  %2 = getelementptr inbounds %"struct.trie::TrieNode<()>"* %0, i32 0, i32 0
  store i32 0, i32* %2
  %3 = getelementptr inbounds %"struct.trie::TrieNode<()>"* %0, i32 0, i32 1
  %4 = getelementptr inbounds [16 x %"enum.trie::Child<()>"]* %3, i32 0, i32 0
  %5 = getelementptr inbounds %"enum.trie::Child<()>"* %4, i32 0
  %6 = getelementptr inbounds %"enum.trie::Child<()>"* %5, i32 0, i32 0
  store i8 2, i8* %6
  %7 = getelementptr inbounds %"enum.trie::Child<()>"* %4, i32 1
  %8 = getelementptr inbounds %"enum.trie::Child<()>"* %7, i32 0, i32 0
  store i8 2, i8* %8
  %9 = getelementptr inbounds %"enum.trie::Child<()>"* %4, i32 2
  %10 = getelementptr inbounds %"enum.trie::Child<()>"* %9, i32 0, i32 0
  store i8 2, i8* %10
  %11 = getelementptr inbounds %"enum.trie::Child<()>"* %4, i32 3
  %12 = getelementptr inbounds %"enum.trie::Child<()>"* %11, i32 0, i32 0
  store i8 2, i8* %12
  %13 = getelementptr inbounds %"enum.trie::Child<()>"* %4, i32 4
  %14 = getelementptr inbounds %"enum.trie::Child<()>"* %13, i32 0, i32 0
  store i8 2, i8* %14
  %15 = getelementptr inbounds %"enum.trie::Child<()>"* %4, i32 5
  %16 = getelementptr inbounds %"enum.trie::Child<()>"* %15, i32 0, i32 0
  store i8 2, i8* %16
  %17 = getelementptr inbounds %"enum.trie::Child<()>"* %4, i32 6
  %18 = getelementptr inbounds %"enum.trie::Child<()>"* %17, i32 0, i32 0
  store i8 2, i8* %18
  %19 = getelementptr inbounds %"enum.trie::Child<()>"* %4, i32 7
  %20 = getelementptr inbounds %"enum.trie::Child<()>"* %19, i32 0, i32 0
  store i8 2, i8* %20
  %21 = getelementptr inbounds %"enum.trie::Child<()>"* %4, i32 8
  %22 = getelementptr inbounds %"enum.trie::Child<()>"* %21, i32 0, i32 0
  store i8 2, i8* %22
  %23 = getelementptr inbounds %"enum.trie::Child<()>"* %4, i32 9
  %24 = getelementptr inbounds %"enum.trie::Child<()>"* %23, i32 0, i32 0
  store i8 2, i8* %24
  %25 = getelementptr inbounds %"enum.trie::Child<()>"* %4, i32 10
  %26 = getelementptr inbounds %"enum.trie::Child<()>"* %25, i32 0, i32 0
  store i8 2, i8* %26
  %27 = getelementptr inbounds %"enum.trie::Child<()>"* %4, i32 11
  %28 = getelementptr inbounds %"enum.trie::Child<()>"* %27, i32 0, i32 0
  store i8 2, i8* %28
  %29 = getelementptr inbounds %"enum.trie::Child<()>"* %4, i32 12
  %30 = getelementptr inbounds %"enum.trie::Child<()>"* %29, i32 0, i32 0
  store i8 2, i8* %30
  %31 = getelementptr inbounds %"enum.trie::Child<()>"* %4, i32 13
  %32 = getelementptr inbounds %"enum.trie::Child<()>"* %31, i32 0, i32 0
  store i8 2, i8* %32
  %33 = getelementptr inbounds %"enum.trie::Child<()>"* %4, i32 14
  %34 = getelementptr inbounds %"enum.trie::Child<()>"* %33, i32 0, i32 0
  store i8 2, i8* %34
  %35 = getelementptr inbounds %"enum.trie::Child<()>"* %4, i32 15
  %36 = getelementptr inbounds %"enum.trie::Child<()>"* %35, i32 0, i32 0
  store i8 2, i8* %36
  ret void
}
; Function Attrs: inlinehint uwtable
define internal %"enum.libc::types::common::c95::c_void"* @_ZN7reflect14MovePtrAdaptor5align4anon7expr_fn6zxa7a9E({ i32, %tydesc*, i8*, i8*, i8 }*, %"enum.libc::types::common::c95::c_void"*) unnamed_addr #4 {
; CHECK-LABEL: @_ZN7reflect14MovePtrAdaptor5align4anon7expr_fn6zxa7a9E
"function top level":
  %__arg = alloca %"enum.libc::types::common::c95::c_void"*
  %p = alloca %"enum.libc::types::common::c95::c_void"*
; CHECK: %__debuginfo_env_ptr = alloca i32**
  %__debuginfo_env_ptr = alloca { i32* }*
  %2 = alloca i32
  store %"enum.libc::types::common::c95::c_void"* %1, %"enum.libc::types::common::c95::c_void"** %__arg
  %3 = load %"enum.libc::types::common::c95::c_void"** %__arg
  store %"enum.libc::types::common::c95::c_void"* %3, %"enum.libc::types::common::c95::c_void"** %p
  %4 = bitcast { i32, %tydesc*, i8*, i8*, i8 }* %0 to { i32, %tydesc*, i8*, i8*, { i32* } }*
  %5 = getelementptr inbounds { i32, %tydesc*, i8*, i8*, { i32* } }* %4, i32 0, i32 4
  store { i32* }* %5, { i32* }** %__debuginfo_env_ptr
  %6 = getelementptr inbounds { i32* }* %5, i32 0, i32 0
  %7 = load i32** %6
  %8 = load %"enum.libc::types::common::c95::c_void"** %p
  %9 = ptrtoint %"enum.libc::types::common::c95::c_void"* %8 to i32
  %10 = load i32* %7
  %11 = load i32* %7
  store i32 %11, i32* %2
  %12 = load i32* %2
  %13 = inttoptr i32 %12 to %"enum.libc::types::common::c95::c_void"*
  ret %"enum.libc::types::common::c95::c_void"* %13
}

define internal void @"_ZN142unboxed_vec$LT$option..Option$LT$$LP$$RP$libc..types..common..c95..c_void$C$$UP$local_data..LocalData.Send$C$local_data..LoanState$RP$$GT$$GT$9glue_drop67hdc2acc74788e8d0863032f96df4ac96f58cf283c1470b3e70ab84f6398ec1bdeaJE"({}*, { i32, i32, [0 x %"enum.option::Option<(*libc::types::common::c95::c_void,~local_data::LocalData:Send,local_data::LoanState)>"] }*) unnamed_addr {
"function top level":
  %2 = getelementptr inbounds { i32, i32, [0 x %"enum.option::Option<(*libc::types::common::c95::c_void,~local_data::LocalData:Send,local_data::LoanState)>"] }* %1, i32 0, i32 0
  %3 = load i32* %2
  %4 = getelementptr inbounds { i32, i32, [0 x %"enum.option::Option<(*libc::types::common::c95::c_void,~local_data::LocalData:Send,local_data::LoanState)>"] }* %1, i32 0, i32 2, i32 0
  %5 = bitcast %"enum.option::Option<(*libc::types::common::c95::c_void,~local_data::LocalData:Send,local_data::LoanState)>"* %4 to i8*
  %6 = getelementptr inbounds i8* %5, i32 %3
  %7 = bitcast i8* %6 to %"enum.option::Option<(*libc::types::common::c95::c_void,~local_data::LocalData:Send,local_data::LoanState)>"*
  br label %iter_vec_loop_header

iter_vec_loop_header:                             ; preds = %iter_vec_loop_body, %"function top level"
  %8 = phi %"enum.option::Option<(*libc::types::common::c95::c_void,~local_data::LocalData:Send,local_data::LoanState)>"* [ %4, %"function top level" ], [ %10, %iter_vec_loop_body ]
  %9 = icmp ult %"enum.option::Option<(*libc::types::common::c95::c_void,~local_data::LocalData:Send,local_data::LoanState)>"* %8, %7
  br i1 %9, label %iter_vec_loop_body, label %iter_vec_next

iter_vec_loop_body:                               ; preds = %iter_vec_loop_header
  %10 = getelementptr inbounds %"enum.option::Option<(*libc::types::common::c95::c_void,~local_data::LocalData:Send,local_data::LoanState)>"* %8, i32 1
  br label %iter_vec_loop_header

iter_vec_next:                                    ; preds = %iter_vec_loop_header
  ret void
}
define internal void @"_ZN51_$x5btrie..Child$LT$$LP$$RP$$GT$$C$$x20..$x2016$x5d9glue_drop67hf75fc21ffe9fcb2cdb87b4fc93776499375d61a8fd9b32988157b4dec027e4e8arE"({}*, [16 x %"enum.trie::Child<()>"]*) unnamed_addr {
"function top level":
  %2 = getelementptr inbounds [16 x %"enum.trie::Child<()>"]* %1, i32 0, i32 0
  %3 = bitcast %"enum.trie::Child<()>"* %2 to i8*
  %4 = getelementptr inbounds i8* %3, i32 128
  %5 = bitcast i8* %4 to %"enum.trie::Child<()>"*
  br label %iter_vec_loop_header

iter_vec_loop_header:                             ; preds = %iter_vec_loop_body, %"function top level"
  %6 = phi %"enum.trie::Child<()>"* [ %2, %"function top level" ], [ %8, %iter_vec_loop_body ]
  %7 = icmp ult %"enum.trie::Child<()>"* %6, %5
  br i1 %7, label %iter_vec_loop_body, label %iter_vec_next

iter_vec_loop_body:                               ; preds = %iter_vec_loop_header
  %8 = getelementptr inbounds %"enum.trie::Child<()>"* %6, i32 1
  br label %iter_vec_loop_header

iter_vec_next:                                    ; preds = %iter_vec_loop_header
  ret void
}

%"enum.std::fmt::rt::Piece[#1].144.758.5056.5670.6284.6898.8126.9824" = type { i8, [7 x i8], [9 x i64] }

@_ZN2io6signal23Signum...std..fmt..Show3fmt15__STATIC_FMTSTR20h2a1052ff3a7cf82bd1K9v0.11.preE = external unnamed_addr constant { { i8, { i8*, i64 }, [56 x i8] } }

; Function Attrs: uwtable
define void @_ZN2io6signal23Signum...std..fmt..Show3fmt20h29346a7683b2d561b0K9v0.11.preE() unnamed_addr {
case_body17:                                      ; preds = %entry-block
  %0 = icmp eq %"enum.std::fmt::rt::Piece[#1].144.758.5056.5670.6284.6898.8126.9824"* bitcast ({ { i8, { i8*, i64 }, [56 x i8] } }* @_ZN2io6signal23Signum...std..fmt..Show3fmt15__STATIC_FMTSTR20h2a1052ff3a7cf82bd1K9v0.11.preE to %"enum.std::fmt::rt::Piece[#1].144.758.5056.5670.6284.6898.8126.9824"*), bitcast (i8* getelementptr inbounds ({ { i8, { i8*, i64 }, [56 x i8] } }* @_ZN2io6signal23Signum...std..fmt..Show3fmt15__STATIC_FMTSTR20h2a1052ff3a7cf82bd1K9v0.11.preE, i64 1, i32 0, i32 0) to %"enum.std::fmt::rt::Piece[#1].144.758.5056.5670.6284.6898.8126.9824"*)
  unreachable
}
; CHECK-LABEL: define void @_ZN2io6signal23Signum...std..fmt..Show3fmt20h29346a7683b2d561b0K9v0.11.preE()
; CHECK-LABEL: case_body17:
; CHECK-NEXT: %0 = icmp eq %"enum.std::fmt::rt::Piece[#1].144.758.5056.5670.6284.6898.8126.9824.7"* bitcast (%0* @_ZN2io6signal23Signum...std..fmt..Show3fmt15__STATIC_FMTSTR20h2a1052ff3a7cf82bd1K9v0.11.preE to %"enum.std::fmt::rt::Piece[#1].144.758.5056.5670.6284.6898.8126.9824.7"*), bitcast (i8* getelementptr inbounds (%0* @_ZN2io6signal23Signum...std..fmt..Show3fmt15__STATIC_FMTSTR20h2a1052ff3a7cf82bd1K9v0.11.preE, i64 1, i32 0) to %"enum.std::fmt::rt::Piece[#1].144.758.5056.5670.6284.6898.8126.9824.7"*)

; ----------------------------------------------------
; promotion invalidates BasicBlock iterator:
; we're most only checking that we don't crash, so there's not much CHECKing that needs to be done here.
define void @_ZN3str7is_utf820ha808b5c55e670da7J6p9v0.11.preE() unnamed_addr {
entry-block:
  br i1 undef, label %_ZN3str28run_utf8_validation_iterator20hab95ed805223fde8J2p9v0.11.preE.exit, label %"_ZN5slice57Items$LT$$x27a$C$$x20T$GT$.Iterator$LT$$BP$$x27a$x20T$GT$4next20hcc6dbb16f27a1a944mo9v0.11.preE.exit.i"

"_ZN5slice57Items$LT$$x27a$C$$x20T$GT$.Iterator$LT$$BP$$x27a$x20T$GT$4next20hcc6dbb16f27a1a944mo9v0.11.preE.exit.i": ; preds = %loop_body.backedge.i, %entry-block
  %0 = phi i8* [ %3, %loop_body.backedge.i ], [ undef, %entry-block ]
  %1 = getelementptr inbounds i8* %0, i64 1
  br i1 undef, label %_ZN3str28run_utf8_validation_iterator20hab95ed805223fde8J2p9v0.11.preE.exit, label %match_case.i

match_case.i:                                     ; preds = %"_ZN5slice57Items$LT$$x27a$C$$x20T$GT$.Iterator$LT$$BP$$x27a$x20T$GT$4next20hcc6dbb16f27a1a944mo9v0.11.preE.exit.i"
  br i1 undef, label %then-block-61106-.i, label %loop_body.backedge.i

then-block-61106-.i:                              ; preds = %match_case.i
  br i1 undef, label %_ZN3str28run_utf8_validation_iterator20hab95ed805223fde8J2p9v0.11.preE.exit, label %match_case7.i

match_case7.i:                                    ; preds = %then-block-61106-.i
  %2 = getelementptr inbounds i8* %0, i64 2
  br i1 undef, label %loop_body.backedge.i, label %_ZN3str28run_utf8_validation_iterator20hab95ed805223fde8J2p9v0.11.preE.exit

loop_body.backedge.i:                             ; preds = %match_case7.i, %match_case.i
  %3 = phi i8* [ %1, %match_case.i ], [ %2, %match_case7.i ]
  br i1 undef, label %_ZN3str28run_utf8_validation_iterator20hab95ed805223fde8J2p9v0.11.preE.exit, label %"_ZN5slice57Items$LT$$x27a$C$$x20T$GT$.Iterator$LT$$BP$$x27a$x20T$GT$4next20hcc6dbb16f27a1a944mo9v0.11.preE.exit.i"

_ZN3str28run_utf8_validation_iterator20hab95ed805223fde8J2p9v0.11.preE.exit: ; preds = %loop_body.backedge.i, %match_case7.i, %then-block-61106-.i, %"_ZN5slice57Items$LT$$x27a$C$$x20T$GT$.Iterator$LT$$BP$$x27a$x20T$GT$4next20hcc6dbb16f27a1a944mo9v0.11.preE.exit.i", %entry-block
  ret void
}

%"enum.std::option::Option<&'static f64>[#1].146.1374.5058.5526" = type { double* }
; CHECK-LABEL: define void @"_ZN6option15Option$LT$T$GT$6unwrap20h2784a1bb46bb36c6IQa4v0.0E"()
define void @"_ZN6option15Option$LT$T$GT$6unwrap20h2784a1bb46bb36c6IQa4v0.0E"() unnamed_addr {
entry-block:
; CHECK: %self = alloca double*
  %self = alloca %"enum.std::option::Option<&'static f64>[#1].146.1374.5058.5526"
  br i1 undef, label %match_case, label %match_else

match_else:                                       ; preds = %entry-block
  unreachable

match_case:                                       ; preds = %entry-block
; CHECK-NOT: %0 = getelementptr inbounds %"enum.std::option::Option<&'static f64>[#1].146.1374.5058.5526"* %self, i32 0, i32 0
  %0 = getelementptr inbounds %"enum.std::option::Option<&'static f64>[#1].146.1374.5058.5526"* %self, i32 0, i32 0
  ret void
}

%"struct.std::raw::Box<()>[#1].67.3137.7435.10505.13575.16645.18487.24560" = type { i64, void (i8*)*, %"struct.std::raw::Box<()>[#1].67.3137.7435.10505.13575.16645.18487.24560"*, %"struct.std::raw::Box<()>[#1].67.3137.7435.10505.13575.16645.18487.24560"*, {} }

define void @_ZN7cleanup10annihilate20hbc4b5b6d732db483yMR9v0.11.preE() unnamed_addr {
entry-block:
  br i1 undef, label %enum-iter-variant-1.i.i.i22, label %_ZN2rt10local_heap11live_allocs20h00ba74d6dc27c595Uz99v0.11.preE.exit27

enum-iter-variant-1.i.i.i22:                      ; preds = %entry-block
  unreachable

_ZN2rt10local_heap11live_allocs20h00ba74d6dc27c595Uz99v0.11.preE.exit27: ; preds = %entry-block
  br i1 undef, label %_ZN7cleanup15each_live_alloc20he6e5fd88bcee2ce4lLR9v0.11.preE.exit, label %while_body.i

while_body.i:                                     ; preds = %while_body.i, %_ZN2rt10local_heap11live_allocs20h00ba74d6dc27c595Uz99v0.11.preE.exit27
  %alloc.01.i = phi %"struct.std::raw::Box<()>[#1].67.3137.7435.10505.13575.16645.18487.24560"* [ %1, %while_body.i ], [ undef, %_ZN2rt10local_heap11live_allocs20h00ba74d6dc27c595Uz99v0.11.preE.exit27 ]
  %0 = getelementptr inbounds %"struct.std::raw::Box<()>[#1].67.3137.7435.10505.13575.16645.18487.24560"* %alloc.01.i, i64 0, i32 3
  %1 = load %"struct.std::raw::Box<()>[#1].67.3137.7435.10505.13575.16645.18487.24560"** %0, align 8
  br i1 undef, label %_ZN7cleanup15each_live_alloc20he6e5fd88bcee2ce4lLR9v0.11.preE.exit, label %while_body.i

_ZN7cleanup15each_live_alloc20he6e5fd88bcee2ce4lLR9v0.11.preE.exit: ; preds = %while_body.i, %_ZN2rt10local_heap11live_allocs20h00ba74d6dc27c595Uz99v0.11.preE.exit27
  unreachable
}

;----------------------------------------------------
; check that we don't skip the instructions when the prior instruction is removed/replaced/etc.
%"struct.fmt::parse::SelectArm.468.3538.6608.13362.15204.16580" = type { %str_slice.1.3071.6141.12895.14737.16578, { i64, i64, [0 x %"enum.fmt::parse::Piece.459.3529.6599.13353.15195.16579"] }* }
%str_slice.1.3071.6141.12895.14737.16578 = type { i8*, i64 }
%"enum.fmt::parse::Piece.459.3529.6599.13353.15195.16579" = type { i8, [7 x i8], [15 x i64] }
; CHECK-NOT: %"enum.fmt::parse::Piece.459.3529.6599.13353.15195.16579" = type { i8, [7 x i8], [15 x i64] }

define void @"_ZN3fmt5parse34Method$LT$$x27a$GT$...std..cmp..Eq2eq20h59da4f9eb7e11a93cjQ9v0.11.preE"() unnamed_addr {
; CHECK-LABEL: define void @"_ZN3fmt5parse34Method$LT$$x27a$GT$...std..cmp..Eq2eq20h59da4f9eb7e11a93cjQ9v0.11.preE"()
entry-block:
  br i1 undef, label %before_rhs.i.i61, label %join24

before_rhs.i.i61:                                 ; preds = %entry-block
  %0 = getelementptr inbounds { i64, i64, [0 x %"struct.fmt::parse::SelectArm.468.3538.6608.13362.15204.16580"] }* undef, i64 0, i32 2, i64 0
; CHECK-NOT: %0 = getelementptr inbounds { i64, i64, [0 x %"struct.fmt::parse::SelectArm.468.3538.6608.13362.15204.16580"] }* undef, i64 0, i32 2, i64 0
  %.209 = select i1 undef, %"struct.fmt::parse::SelectArm.468.3538.6608.13362.15204.16580"* null, %"struct.fmt::parse::SelectArm.468.3538.6608.13362.15204.16580"* %0
; CHECK: %.209 = select i1 undef, %"struct.fmt::parse::SelectArm.468.3538.6608.13362.15204.16580.9"* null, %"struct.fmt::parse::SelectArm.468.3538.6608.13362.15204.16580.9"* %1
  br label %join24

join24:                                           ; preds = %before_rhs.i.i61, %entry-block
  ret void
}
%"enum.fmt::parse::Piece.459.3529.6599.13353.17192" = type { i8, [7 x i8], [15 x i64] }

declare void @llvm.lifetime.start(i64, i8* nocapture)

define void @"_ZN3fmt5parse34Method$LT$$x27a$GT$...std..cmp..Eq2eq20h59da4f9eb7e11a93cjQ9v0.11.preE2"() unnamed_addr {
; CHECK-LABEL: define void @"_ZN3fmt5parse34Method$LT$$x27a$GT$...std..cmp..Eq2eq20h59da4f9eb7e11a93cjQ9v0.11.preE2"()
"_ZN3fmt5parse37SelectArm$LT$$x27a$GT$...std..cmp..Eq2eq20ha51847e9e7f8b742xuQ9v0.11.preE.exit.i":
  call void @llvm.lifetime.start(i64 16, i8* undef) #0
  %0 = getelementptr inbounds { i64, i64, [0 x %"enum.fmt::parse::Piece.459.3529.6599.13353.17192"] }* undef, i64 0, i32 2, i64 0
; CHECK-NOT: %0 = getelementptr inbounds { i64, i64, [0 x %"enum.fmt::parse::Piece.459.3529.6599.13353.17192"] }* undef, i64 0, i32 2, i64 0
  store %"enum.fmt::parse::Piece.459.3529.6599.13353.17192"* %0, %"enum.fmt::parse::Piece.459.3529.6599.13353.17192"** undef, align 8
; CHECK: store %"enum.fmt::parse::Piece.459.3529.6599.13353.17192.12"* %1, %"enum.fmt::parse::Piece.459.3529.6599.13353.17192.12"** undef, align 8
  unreachable
}

;---------------------------------------------------
