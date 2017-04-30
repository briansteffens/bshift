.PHONY: bshift test clean
default: bin/bshift

bin/bshift: src/main.d src/globals.d src/lexer.d src/ast.d src/parser.d src/generator.d
	ldc -g -of=bin/bshift $^

clean:
	rm -rf bin/bshift bin/bshift.o *.asm *.o lib/*.asm lib/*.o
