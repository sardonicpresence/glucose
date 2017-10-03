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

declare dllimport x86_stdcallcc void @ExitProcess(i32) nounwind noreturn

@STD_INPUT_HANDLE = private unnamed_addr constant i32 -10
@STD_OUTPUT_HANDLE = private unnamed_addr constant i32 -11
@STD_ERROR_HANDLE = private unnamed_addr constant i32 -12
declare dllimport x86_stdcallcc i8* @GetStdHandle(i32)

declare dllimport x86_stdcallcc i1 @WriteFile(i8* nonnull, i8* nonnull, i32, i32*, i8*)

declare fastcc void @$initAlloc(%$size %nurseryBytes) unnamed_addr #0

declare fastcc i32 @main(%$box) unnamed_addr #0

define void @_start() unnamed_addr norecurse noreturn {
  tail call fastcc void @$initAlloc(%$size 262144)
  %1 = call fastcc i32 @main(%$box null)
  tail call x86_stdcallcc void @ExitProcess(i32 %1)
  ret void
}

define fastcc i32 @$strlen([0 x i8]* nonnull %s) unnamed_addr #0 norecurse {
  br label %Loop
Loop:
  %1 = phi i32 [0, %0], [%5, %Loop]
  %2 = getelementptr inbounds [0 x i8], [0 x i8]* %s, i64 0, i32 %1
  %3 = load i8, i8* %2
  %4 = icmp eq i8 %3, 0
  %5 = add i32 %1, 1
  br i1 %4, label %Done, label %Loop
Done:
  ret i32 %1
}

define fastcc void @$abort([0 x i8]* nonnull %message) unnamed_addr #0 norecurse noreturn {
  %1 = load i32, i32* @STD_ERROR_HANDLE
  %2 = call x86_stdcallcc i8* @GetStdHandle(i32 %1)
  %3 = getelementptr inbounds [0 x i8], [0 x i8]* %message, i64 0, i64 0
  %4 = call fastcc i32 @$strlen([0 x i8]* %message)
  call x86_stdcallcc i1 @WriteFile(i8* %2, i8* %3, i32 %4, i32* null, i8* null)
  tail call x86_stdcallcc void @ExitProcess(i32 1)
  ret void
}

attributes #0 = { nounwind align=16 }
