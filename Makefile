.PHONY: bshift test clean
default: bin/bshift

bin/bshift: src/main.d src/globals.d src/lexer.d src/ast.d src/parser.d src/validator.d src/generator.d
	ldc -g -of=bin/bshift $^

install:
	ln -sf $(shell pwd)/bin/bshift /usr/local/bin
	ln -sf $(shell pwd)/lib /usr/local/lib/bshift

clean:
	rm -rf bin/bshift bin/bshift.o *.asm *.o lib/*.asm lib/*.o
