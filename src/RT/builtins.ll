target datalayout = "e-m:w-i64:64-n8:16:32:64-S128"
target triple = "x86_64-pc-windows"

%$size = type i64

; TODO: Consider taking advantage of alignment somehow
; TODO: Based solely on name, LLVM refuses to eliminate this function if unused!
define fastcc void @memcpy(i8* %to, i8* %from, %$size %bytes) unnamed_addr #0 {
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
