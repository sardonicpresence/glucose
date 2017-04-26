; Align all pointers (functions and data) to 16 bytes (128 bits)?
;  - leaves 4 LSB of every pointer for tags

target datalayout = "e-m:w-i64:64-n8:16:32:64-S128"
target triple = "x86_64-pc-windows"

%$boxed = type opaque
%$box = type %$boxed*
%$size = type i64
%$arity = type i16
%$argsize = type i32
%$fn = type opaque

%$closure = type {
  %$fn*, ; Code pointer + tag bits (used for..?)
  %$arity, ; Number of unbound arguments
  %$arity, ; Number of pointer args that follow
  %$argsize, ; Number of bytes of non-pointer args that follow
  [0 x %$box], ; Variable number of bound pointer arguments (assumed to be 16-byte aligned)
  <{}> ; Variable number of bound non-pointer arguments
}

@MEM_COMMIT = private unnamed_addr constant i32 4096
@MEM_RESERVE = private unnamed_addr constant i32 8192
@MEM_RESERVE_COMMIT = private unnamed_addr constant i32 12288
@MEM_READWRITE = private unnamed_addr constant i32 4
declare dllimport i8* @VirtualAlloc(i8*, i64, i32, i32)
declare dllimport void @ExitProcess(i32) nounwind ; noreturn produces less minimal instructions in _start

declare %$box @main(%$box) unnamed_addr nounwind

define void @_start() unnamed_addr norecurse noreturn nounwind {
  %1 = call %$box @main(%$box null)
  %2 = bitcast %$box %1 to i32*
  %3 = load i32, i32* %2
  tail call void @ExitProcess(i32 %3)
  ret void
}

define nonnull noalias align 16 %$box* @$heapAlloc(%$size %bytes) unnamed_addr allocsize(0) nounwind align 16 {
  %1 = call i8* @VirtualAlloc(i8* null, %$size %bytes, i32 12288, i32 4)
  %2 = bitcast i8* %1 to %$box*
  ret %$box* %2
}

; TODO: Consider taking advantage of alignment somehow
define void @memcpy(i8* %to, i8* %from, %$size %bytes) unnamed_addr align 16 nounwind {
  %1 = icmp eq %$size %bytes, 0
  br i1 %1, label %Done, label %Loop
Loop:
  %2 = phi %$size [0, %0], [%6, %Loop]
  %3 = getelementptr inbounds i8, i8* %from, %$size %2
  %4 = load i8, i8* %3
  %5 = getelementptr inbounds i8, i8* %to, %$size %2
  store i8 %4, i8* %5
  %6 = add %$size %2, 1
  %7 = icmp eq %$size %6, %bytes
  br i1 %7, label %Done, label %Loop
Done:
  ret void
}
