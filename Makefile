build :
	stack build --fast --exec "glucose example.glc" --exec "glucose -t js example.glc" && \
	opt -O3 example.ll | llc -O3 > example.s && \
	as example.s -o example.o

test :
	stack test --fast --test-arguments "-f progress"

doc :
	make -C manual html man

.PHONY : build test doc
