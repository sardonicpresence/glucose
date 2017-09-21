BIN=$(shell cygpath -m `stack path --local-install-root`)/bin

# PASSES=-inline -loop-unroll -gvn -simplifycfg -barrier -instcombine
PASSES=-O3

default : example.js example.exe

# example.exe : example.o src\RT\rt.o
# 	ld -O --gc-sections -entry _start $^ -lkernel32 -o $@

%.exe : %.bc src/RT/rt.bc
	lld-link $^ /subsystem:windows /entry:_start /out:$@ /debug /defaultlib:kernel32 /mllvm:-mcpu=broadwell /mllvm:-O3

%.o : %.s
	llvm-mc -arch=x86-64 -mc-relax-all -mcpu=broadwell -filetype=obj $^ -o $@

%.bc : %.ll
	opt $(PASSES) $^ -o=$@ -mcpu=broadwell

%.s : %.bc
	llc -O3 $^ -o=$@ -mcpu=broadwell

%.ll : %.glc $(BIN)/glucose.exe
	stack exec glucose -- $<

%.js : %.glc $(BIN)/glucose.exe
	stack exec glucose -- -t js $<

$(BIN)/glucose.exe : build
	echo $@

build :
	stack build --fast

test :
	stack test --fast --test-arguments "-f progress"

doc :
	make -C manual html man

clean :
	stack clean
	rm -f example.exe example.o example.bc example.s example.ll example.opt example.js src/RT/rt.s src/RT/rt.bc src/RT/rt.o

.PHONY : default build test doc clean

.PRECIOUS : %.ll %.opt %.s %.o
