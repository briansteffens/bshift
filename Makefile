.PHONY: install clean

bin/bshift: src/*.d
	ldc -g -of=$@ $^

install:
	ln -sf $(shell pwd)/bin/bshift /usr/local/bin
	ln -sf $(shell pwd)/lib /usr/local/lib/bshift

clean:
	rm -rf bin/bshift bin/bshift.o *.asm *.o lib/*.asm lib/*.o
