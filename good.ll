target datalayout = "e-m:w-i64:64-n8:16:32:64-S128"
target triple = "x86_64-pc-windows"

%$boxed = type opaque
%$box = type %$boxed*
%$p = type i64
%$arity = type i16
%$fn = type opaque

%$slowI = type i32(%$box*)

@_test = unnamed_addr constant double 5432.1
@foo = unnamed_addr constant i32 1000
@bar = unnamed_addr alias i32, i32* @foo

define i32 @three(%$box %a) unnamed_addr nounwind {
  ret i32 3
}

define %$box @id(%$box %a) unnamed_addr nounwind {
  ret %$box %a
}

define %$box @_const(%$box %a, %$box %b) unnamed_addr nounwind {
  ret %$box %a
}

define double @test(%$box %a) unnamed_addr nounwind {
  %1 = load double, double* @_test
  ret double %1
}

define %$box @test2(%$box %a) unnamed_addr nounwind {
  %1 = call %$box @_const (%$box bitcast(double (%$box)* @test to %$box), %$box %a)
  ret %$box %1
}

define %$box @test4(%$box %a, %$box %b) unnamed_addr nounwind {
  %1 = call %$box @_const (%$box bitcast(%$box (%$box, %$box)* @_const to %$box), %$box %a)
  %2 = bitcast %$box %1 to %$box (%$box, %$box)*
  %3 = call %$box %2 (%$box bitcast(%$box (%$box)* @test2 to %$box), %$box %b)
  ret %$box %3
}

@$tag = unnamed_addr constant %$p 7

%$closure = type {
  %$fn*, ; Code pointer + tag bits (used for..?)
  %$arity, ; Arity of the function addressed by `code`
  %$arity, ; Number of args that follow
  [0 x %$box] ; Variable number of bound arguments
}

declare external nonnull %$box* @$heapAlloc(i64) allocsize(0) nounwind

define linkonce_odr %$box* @$prepareClosure(%$closure* nonnull %clp, %$arity %nargs, %$box* nonnull %args2) unnamed_addr nounwind {
  %args = getelementptr inbounds %$closure, %$closure* %clp, i64 0, i32 3
  br label %Loop
Loop:
  %idx = phi %$arity [0, %0], [%next, %Loop]
  %from = getelementptr inbounds [0 x %$box], [0 x %$box]* %args, i64 0, %$arity %idx
  %to = getelementptr inbounds %$box, %$box* %args2, %$arity %idx
  %arg = load %$box, %$box* %from, align 8
  store %$box %arg, %$box* %to, align 8
  %next = add %$arity %idx, 1
  %done = icmp eq %$arity %next, %nargs
  br i1 %done, label %Done, label %Loop
Done:
  %remainder = getelementptr inbounds %$box, %$box* %args2, %$arity %nargs
  ret %$box* %remainder
}

; define private fastcc i32 @$applyI_P(%$box nonnull %a, %$fn* %f) unnamed_addr nounwind {
;   %1 = ptrtoint %$fn* %f to %$p
;   %2 = icmp eq %$p %1, 0
;   br i1 %2, label %A, label %B
; A:
;   ret i32 0
; B:
;   ret i32 1
; }

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
  %target = bitcast %$fn* %target0 to %$slowI*
  %res = call i32 %target(%$box* %args2)
  ret i32 %res
}

define private i32 @test5$$1$$(%$box* nonnull %pargs) unnamed_addr nounwind {
  %pa = getelementptr %$box, %$box* %pargs, i64 0
  %pa2 = bitcast %$box* %pa to i32*
  %a = load i32, i32* %pa2, align 8

  %pc = getelementptr %$box, %$box* %pargs, i64 1
  %c = load %$box, %$box* %pc, align 8

  %result = call i32 @test5$$1(i32 %a, %$box %c)
  ret i32 %result
}

define private i32 @test5$$1(i32 %a, %$box nonnull %c) unnamed_addr nounwind {
  %pa = alloca i32
  store i32 %a, i32* %pa
  %1 = bitcast i32* %pa to %$box
  %2 = call %$box @_const (%$box %1, %$box %c)
  %3 = bitcast %$box %2 to i32*
  %4 = load i32, i32* %3
  ret i32 %4
}

define i32 @test5(i32 %a, %$box nonnull %b) unnamed_addr nounwind {
  %cl0 = alloca { %$fn*, %$arity, %$arity, [1 x %$box] }
  %cl = bitcast { %$fn*, %$arity, %$arity, [1 x %$box] }* %cl0 to %$closure*

  %lambda = bitcast %$slowI* @test5$$1$$ to %$fn*
  %clpf = getelementptr inbounds %$closure, %$closure* %cl, i64 0, i32 0
  store %$fn* %lambda, %$fn** %clpf

  %clpn = getelementptr inbounds %$closure, %$closure* %cl, i64 0, i32 1
  store %$arity 2, %$arity* %clpn

  %clpm = getelementptr inbounds %$closure, %$closure* %cl, i64 0, i32 2
  store %$arity 1, %$arity* %clpm

  %pa = getelementptr %$closure, %$closure* %cl, i64 0, i32 3, i64 0
  %pa2 = bitcast %$box* %pa to i32*
  store i32 %a, i32* %pa2

  %1 = bitcast %$closure* %cl to %$box
  %2 = call %$box @id (%$box %1)

  %arg = bitcast %$box %2 to %$fn*
  %result = call i32 @$applyI_P(%$box %b, %$fn* %arg)
  ret i32 %result
}

declare void @llvm.memcpy.p0i8.p0i8.i64(i8*, i8*, i64, i32, i1) argmemonly nounwind
declare void @llvm.lifetime.start(i64, i8* nocapture) argmemonly nounwind
declare void @llvm.lifetime.end(i64, i8* nocapture) argmemonly nounwind
