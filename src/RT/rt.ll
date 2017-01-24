; Align all pointers (functions and data) to 16 bytes (128 bits)?
;  - leaves 4 LSB of every pointer for tags

target datalayout = "e-m:w-i64:64-n8:16:32:64-S128"
target triple = "x86_64-pc-windows"

%$boxed = type opaque
%$box = type %$boxed*
%$p = type i64
%$arity = type i16
%$fn = type opaque

@MEM_COMMIT = unnamed_addr constant i32 4096
@MEM_RESERVE = unnamed_addr constant i32 8192
@MEM_READWRITE = unnamed_addr constant i32 4
declare dllimport i8* @VirtualAlloc(i8*, i64, i32, i32);

define nonnull noalias align 8 %$box* @$heapAlloc(i64) allocsize(0) nounwind align 8 {
  ret %$box* null
}

%$closure = type {
  %$fn*, ; Code pointer + tag bits (used for..?)
  %$arity, ; Arity of the function addressed by `code`
  %$arity, ; Number of args that follow
  [0 x %$box] ; Variable number of bound arguments
}
@$tag = unnamed_addr constant %$p 7

define linkonce_odr i32 @$applyI_P(%$box nonnull %a, %$fn* nonnull %f) unnamed_addr nounwind alwaysinline {
  %1 = ptrtoint %$fn* %f to %$p
  %tag = load %$p, %$p* @$tag
  %arity = and %$p %1, %tag
  %isTrivial = icmp eq %$p %arity, 1
  br i1 %isTrivial, label %Trivial, label %Nontrivial
Trivial:
  %fp = bitcast %$fn* %f to i32(%$box)*
  %result = tail call i32 %fp(%$box %a)
  ret i32 %result
Nontrivial:
  %isClosure = icmp eq %$p %arity, 0
  br i1 %isClosure, label %Closure, label %Function
Function:
  unreachable
Closure:
  ; Tag bits were 0 so no need to mask
  %clp = bitcast %$fn* %f to %$closure*
  %pnargs = getelementptr inbounds %$closure, %$closure* %clp, i64 0, i32 2
  %nargs = load %$arity, %$arity* %pnargs
  %args = getelementptr inbounds %$closure, %$closure* %clp, i64 0, i32 3
  %nargs2 = add %$arity %nargs, 1

  %nargs2.i64 = zext %$arity %nargs2 to i64
  %args2 = call %$box* @$heapAlloc(i64 %nargs2.i64)
  %args2.p = bitcast %$box* %args2 to [0 x %$box]*
  %args2.i8 = bitcast [0 x %$box]* %args2.p to i8*

  %args.p = getelementptr [0 x %$box], [0 x %$box]* %args, i64 0
  %args.i8 = bitcast [0 x %$box]* %args.p to i8*
  %bytes = mul %$arity %nargs, 8
  %bytes.i64 = zext %$arity %bytes to i64
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %args2.i8, i8* %args.i8, i64 %bytes.i64, i32 8, i1 false)

  %pa = getelementptr inbounds [0 x %$box], [0 x %$box]* %args2.p, i64 0, %$arity %nargs

  store %$box %a, %$box* %pa
  %ptarget = getelementptr inbounds %$closure, %$closure* %clp, i64 0, i32 0
  %target0 = load %$fn*, %$fn** %ptarget, align 8
  %target = bitcast %$fn* %target0 to i32(%$box*)*
  %res = call i32 %target(%$box* %args2)
  ret i32 %res
}

declare void @llvm.memcpy.p0i8.p0i8.i64(i8*, i8*, i64, i32, i1) argmemonly nounwind
