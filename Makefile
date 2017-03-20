BIN=$(shell cygpath -m `stack path --local-install-root`)/bin

default : example.js example.exe

# example.exe : example.o src\RT\rt.o
# 	ld -O --gc-sections -entry _start $^ -lkernel32 -o $@

example.exe : example.bc src/RT/rt.bc
	lld-link $^ /subsystem:windows /entry:_start /out:$@ /debug /defaultlib:kernel32

%.o : %.s
	llvm-mc -arch=x86-64 -mc-relax-all -mcpu=broadwell -filetype=obj $^ -o $@

%.bc : %.ll
	opt -O3 $^ -o=$@

%.s : %.bc
	llc -O3 $^ -o=$@

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
	rm -f example.exe example.o example.s example.ll example.opt example.js src/RT/rt.s src/RT/rt.o

.PHONY : default build test doc clean

.PRECIOUS : %.ll %.opt %.s %.o
