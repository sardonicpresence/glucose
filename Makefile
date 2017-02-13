BIN=$(shell cygpath -w `stack path --local-install-root`)\bin

default : example.js example.exe

example.exe : example.o src\RT\rt.o
	ld -O --gc-sections -entry _start $^ -lkernel32 -o $@

%.o : %.s
	llvm-mc -filetype=obj $^ -o $@

%.opt : %.ll
	opt -O3 -S $^ -o=$@

%.s : %.opt
	llc -O3 $^ -o=$@

%.ll : %.glc $(BIN)\glucose.exe
	stack exec glucose -- $<

%.js : %.glc $(BIN)\glucose.exe
	stack exec glucose -- -t js $<

$(BIN)\glucose.exe : build

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
