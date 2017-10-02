BIN=$(shell cygpath -m `stack path --local-install-root`)/bin
LLVM=../llvm/build/bin/

# PASSES=-inline -loop-unroll -gvn -simplifycfg -barrier -instcombine
PASSES=-O3

default : example.js example.exe

# %.exe : %.o src\RT\rt.o
# 	ld -O --gc-sections -entry _start $^ -lkernel32 -o $@

%.exe : %.bc src/RT/rt.bc src/RT/builtins.o
	$(LLVM)lld-link $^ /subsystem:windows /entry:_start /out:$@ /debug /defaultlib:kernel32 /mllvm:-mcpu=broadwell

%.o : %.s
	llvm-mc -arch=x86-64 -mc-relax-all -mcpu=broadwell -filetype=obj $^ -o $@

%.bc : %.ll
	$(LLVM)opt $(PASSES) $^ -o=$@ -mcpu=broadwell

%.s : %.bc
	$(LLVM)llc -O3 $^ -o=$@ -mcpu=broadwell

%.ll : %.glc $(BIN)/glucose.exe
	stack exec glucose -- $<

%.opt.ll : %.bc
	$(LLVM)opt -S $^ >$@

%.js : %.glc $(BIN)/glucose.exe
	stack exec glucose -- -t js $<

$(BIN)/glucose.exe : build
	@true

build :
	stack build --fast

test :
	stack test --fast --test-arguments "-f progress"

doc :
	make -C manual html man

clean :
	stack clean
	rm -f example.exe example.exe.manifest example.pdb example.o example.bc example.s example.ll example.opt.ll example.js
	rm -f src/RT/rt.s src/RT/rt.bc src/RT/rt.o

.PHONY : default build test doc clean

.PRECIOUS : %.ll %.opt.ll %.s %.o %.bc
