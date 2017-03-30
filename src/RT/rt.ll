; Align all pointers (functions and data) to 16 bytes (128 bits)?
;  - leaves 4 LSB of every pointer for tags

target datalayout = "e-m:w-i64:64-n8:16:32:64-S128"
target triple = "x86_64-pc-windows"

%$boxed = type opaque
%$box = type %$boxed*
%$p = type i64
%$arity = type i16
%$fn = type opaque

%$closure = type {
  %$fn*, ; Code pointer + tag bits (used for..?)
  %$arity, ; Arity of the function addressed by `code`
  %$arity, ; Number of args that follow
  [0 x %$box] ; Variable number of bound arguments
}
@$tag = private unnamed_addr constant %$p 7

@MEM_COMMIT = private unnamed_addr constant i32 4096
@MEM_RESERVE = private unnamed_addr constant i32 8192
@MEM_RESERVE_COMMIT = private unnamed_addr constant i32 12288
@MEM_READWRITE = private unnamed_addr constant i32 4
declare dllimport i8* @VirtualAlloc(i8*, i64, i32, i32)
declare dllimport void @ExitProcess(i32) nounwind ; noreturn produces less minimal instructions in _start

declare i32 @main(%$box) unnamed_addr nounwind

define void @_start() unnamed_addr norecurse noreturn nounwind {
  %1 = call i32 @main(%$box null)
  tail call void @ExitProcess(i32 %1)
  ret void
}

define nonnull noalias align 8 %$box* @$heapAlloc(i64 %bytes) allocsize(0) nounwind align 16 {
  %1 = call i8* @VirtualAlloc(i8* null, i64 %bytes, i32 12288, i32 4)
  %2 = bitcast i8* %1 to %$box*
  ret %$box* %2
}
