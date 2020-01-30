; RUN: llc < %s -mtriple i386-unknown-unknown -O1 -stop-before=finalize-isel | FileCheck %s

; Test case is designed to show the differences related to how implicit values
; are handled by SelectionDAGBuilder::EmitFuncArgumentDbgValue.
;
; One purpose is to validate a bugfix in DIExpression::isImplicit() related
; to DIExpressions containing both DW_OP_stack_value and DW_OP_LLVM_fragment.
;
; IR generated by:
;
;   // clang -S -target i386-- -Wall -g -O1 -mllvm -stop-before=finalize-isel
;
;   typedef struct {
;       int x;
;       int *y;
;   } bar;
;
;   bar* func1(bar *p1, long p2, bar p3)
;   {
;      bar *foo1 = p1+18;
;      p3.y = &((p1+18)->x);
;      return p1;
;   }


target datalayout = "e-m:e-p:32:32-p270:32:32-p271:32:32-p272:64:64-f64:32:64-f80:32-n8:16:32-S128"
target triple = "i386-unknown-unknown"

%struct.bar = type { i32, i32* }

; Function Attrs: norecurse nounwind readnone
define dso_local %struct.bar* @func1(%struct.bar* readnone returned %0, i32 %1, i32 %2, i32* nocapture readnone %3) local_unnamed_addr #0 !dbg !8 {
; CHECK-DAG: DBG_VALUE %fixed-stack.1, $noreg, {{.*}}, !DIExpression(DW_OP_deref, DW_OP_LLVM_fragment, 0, 32),
; CHECK-DAG: DBG_VALUE %fixed-stack.0, $noreg, {{.*}}, !DIExpression(DW_OP_deref, DW_OP_LLVM_fragment, 32, 32),
; CHECK-DAG: DBG_VALUE %fixed-stack.3, $noreg, {{.*}}, !DIExpression(DW_OP_deref),
; CHECK-DAG: DBG_VALUE %fixed-stack.2, $noreg, {{.*}}, !DIExpression(DW_OP_deref),
; CHECK-DAG: DBG_VALUE %fixed-stack.3, $noreg, {{.*}}, !DIExpression(DW_OP_deref_size, 4, DW_OP_plus_uconst, 144, DW_OP_stack_value),
; CHECK-DAG: DBG_VALUE %fixed-stack.3, $noreg, {{.*}}, !DIExpression(DW_OP_deref_size, 4, DW_OP_plus_uconst, 144, DW_OP_stack_value, DW_OP_LLVM_fragment, 32, 32),

  call void @llvm.dbg.value(metadata i32 %2, metadata !24, metadata !DIExpression(DW_OP_LLVM_fragment, 0, 32)), !dbg !26
  call void @llvm.dbg.value(metadata i32* %3, metadata !24, metadata !DIExpression(DW_OP_LLVM_fragment, 32, 32)), !dbg !26
  call void @llvm.dbg.value(metadata %struct.bar* %0, metadata !22, metadata !DIExpression()), !dbg !26
  call void @llvm.dbg.value(metadata i32 %1, metadata !23, metadata !DIExpression()), !dbg !26
  call void @llvm.dbg.value(metadata %struct.bar* %0, metadata !25, metadata !DIExpression(DW_OP_plus_uconst, 144, DW_OP_stack_value)), !dbg !26
  call void @llvm.dbg.value(metadata %struct.bar* %0, metadata !24, metadata !DIExpression(DW_OP_plus_uconst, 144, DW_OP_stack_value, DW_OP_LLVM_fragment, 32, 32)), !dbg !26
  ret %struct.bar* %0, !dbg !27
}

; Function Attrs: nounwind readnone speculatable willreturn
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

attributes #0 = { norecurse nounwind readnone }
attributes #1 = { nounwind readnone speculatable willreturn }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!3, !4, !5, !6}
!llvm.ident = !{!7}

!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, producer: "clang version 10.0.0 (trunk 375507)", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug, enums: !2, nameTableKind: None)
!1 = !DIFile(filename: "example.c", directory: "/home/ubuntu")
!2 = !{}
!3 = !{i32 1, !"NumRegisterParameters", i32 0}
!4 = !{i32 2, !"Dwarf Version", i32 4}
!5 = !{i32 2, !"Debug Info Version", i32 3}
!6 = !{i32 1, !"wchar_size", i32 4}
!7 = !{!"clang version 10.0.0 (trunk 375507)"}
!8 = distinct !DISubprogram(name: "func1", scope: !9, file: !9, line: 6, type: !10, scopeLine: 7, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !21)
!9 = !DIFile(filename: "./example.c", directory: "/home/ubuntu")
!10 = !DISubroutineType(types: !11)
!11 = !{!12, !12, !20, !13}
!12 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !13, size: 32)
!13 = !DIDerivedType(tag: DW_TAG_typedef, name: "bar", file: !9, line: 4, baseType: !14)
!14 = distinct !DICompositeType(tag: DW_TAG_structure_type, file: !9, line: 1, size: 64, elements: !15)
!15 = !{!16, !18}
!16 = !DIDerivedType(tag: DW_TAG_member, name: "x", scope: !14, file: !9, line: 2, baseType: !17, size: 32)
!17 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!18 = !DIDerivedType(tag: DW_TAG_member, name: "y", scope: !14, file: !9, line: 3, baseType: !19, size: 32, offset: 32)
!19 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !17, size: 32)
!20 = !DIBasicType(name: "long int", size: 32, encoding: DW_ATE_signed)
!21 = !{!22, !23, !24, !25}
!22 = !DILocalVariable(name: "p1", arg: 1, scope: !8, file: !9, line: 6, type: !12)
!23 = !DILocalVariable(name: "p2", arg: 2, scope: !8, file: !9, line: 6, type: !20)
!24 = !DILocalVariable(name: "p3", arg: 3, scope: !8, file: !9, line: 6, type: !13)
!25 = !DILocalVariable(name: "foo1", scope: !8, file: !9, line: 8, type: !12)
!26 = !DILocation(line: 0, scope: !8)
!27 = !DILocation(line: 10, column: 4, scope: !8)
