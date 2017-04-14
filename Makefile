.PHONY: bshift test clean
default: main

main: main.d lexer.d ast.d parser.d generator.d
	ldc $^

uname := $(shell uname -s)
asm = basm
ifeq ($(uname), Darwin)
	asm = nasm -f macho64
endif

test: test.bs
	./main $<
	$(asm) ${@}.asm
	ld -e _start ${@}.o -o $@
	./$@


clean:
	rm -rf main test *.asm *.o a.out
