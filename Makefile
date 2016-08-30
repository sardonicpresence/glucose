build :
	stack build --fast --exec "glucose example.glc"

test :
	stack test --fast --test-arguments "-f progress"

doc :
	make -C manual html man

.PHONY : build test doc
