build :
	stack build --fast --exec "glucose example.glc" --exec "glucose -t js example.glc"
	llc -O3 example.ll
	as example.s -o example.o

test :
	stack test --fast --test-arguments "-f progress"

doc :
	make -C manual html man

.PHONY : build test doc
