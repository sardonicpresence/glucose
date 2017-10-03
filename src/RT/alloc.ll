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

@MEM_COMMIT = private unnamed_addr constant i32 4096
@MEM_RESERVE = private unnamed_addr constant i32 8192
@MEM_RESERVE_COMMIT = private unnamed_addr constant i32 12288
@MEM_READWRITE = private unnamed_addr constant i32 4
declare dllimport x86_stdcallcc i32* @VirtualAlloc(i32*, i64, i32, i32)

; Number of bytes in (each) nursery
@$nurseryBytes = private unnamed_addr global %$size zeroinitializer

; Pointer to next available allocation in the nursery
@$allocNext = private unnamed_addr global i32* zeroinitializer

; Pointer beyond last memory available for allocation in allocated nursery
@$allocLast = private unnamed_addr global i32* zeroinitializer

define fastcc void @$initAlloc(%$size %nurseryBytes) unnamed_addr #0 {
  store %$size %nurseryBytes, %$size* @$nurseryBytes
  call fastcc i32* @$newNursery()
  ret void
}

define private fastcc nonnull noalias i32* @$newNursery() unnamed_addr alwaysinline #0 {
  %nurseryBytes = load %$size, %$size* @$nurseryBytes
  %1 = load i32, i32* @MEM_RESERVE_COMMIT
  %2 = load i32, i32* @MEM_READWRITE
  %3 = call x86_stdcallcc i32* @VirtualAlloc(i32* null, %$size %nurseryBytes, i32 %1, i32 %2)
  store i32* %3, i32** @$allocNext
  %4 = ptrtoint i32* %3 to %$size
  %5 = sub %$size %nurseryBytes, ptrtoint(i32** getelementptr inbounds (i32*, i32** zeroinitializer, i64 1) to %$size)
  %6 = add %$size %4, %5
  %7 = inttoptr %$size %6 to i32*
  store i32* %7, i32** @$allocLast
  ret i32* %3
}

define private fastcc i32* @$aligned(i32* %ptr, i8 %align) unnamed_addr alwaysinline #0 {
  %1 = ptrtoint i32* %ptr to %$size
  %2 = zext i8 %align to %$size
  %3 = add %$size %1, %2
  %4 = add %$size %3, -1
  %5 = udiv %$size %4, %2
  %6 = mul %$size %5, %2 ; next aligned pointer
  %7 = inttoptr %$size %6 to i32*
  ret i32* %7
}

define private fastcc void @$nurseryAlloc(i32* %ptr, %$size %bytes) unnamed_addr alwaysinline #0 {
  %1 = ptrtoint i32* %ptr to %$size
  %2 = add %$size %1, %bytes
  %3 = inttoptr %$size %2 to i32*
  store i32* %3, i32** @$allocNext
  ret void
}

define fastcc nonnull noalias align 8 %$box @$heapAlloc(%$size %bytes, i8 %align) unnamed_addr #0 allocsize(0) {
  %1 = load i32*, i32** @$allocNext
  %2 = load i32*, i32** @$allocLast
  %3 = call fastcc i32* @$aligned(i32* %1, i8 %align)
  %4 = ptrtoint i32* %3 to %$size
  %5 = add %$size %4, %bytes
  %6 = ptrtoint i32* %2 to %$size
  %7 = icmp ule %$size %5, %6
  br i1 %7, label %Room, label %NoRoom
Room:
  %8 = inttoptr %$size %5 to i32*
  store i32* %8, i32** @$allocNext
  %p = bitcast i32* %3 to %$box
  ret %$box %p
NoRoom:
  ; TODO: trigger GC
  %9 = call fastcc i32* @$newNursery()
  call fastcc void @$nurseryAlloc(i32* %9, %$size %bytes)
  %10 = bitcast i32* %9 to %$box
  ret %$box %10
}

declare fastcc void @$abort([0 x i8]* nonnull %message) unnamed_addr #0 norecurse noreturn

attributes #0 = { nounwind align=16 }
