# bshift

This is a toy compiler for a vaguely C-style language with a couple of hints
from Go.

*Note: don't use this for anything, it's super unstable.*

For an example of a trivial bshift program, take a look at
[show](https://github.com/briansteffens/show). This is the first actual bshift
program.

Some random notes on various peculiarities of the compiler:

- Compiles straight to Intel-style x86-64 assembly, rather than a proper
  compiler backend intermediate representation
- No 32-bit support
- bshift programs don't link to libc by default. bshift has its own interface
  to syscalls and its own (extremely naive) memory manager
- Has a hand-written parser which uses recursive descent and shunting yard




### Dependencies

* [ldc](https://wiki.dlang.org/LDC)
* gcc
* [basm](https://github.com/briansteffens/basm) or nasm
* GNU make





### Usage

```bash
make
bin/bshift examples/hello.bs
./a.out
```


### Running the tests

Additional dependencies for running the tests:

* [asmtest](https://github.com/briansteffens/asmtest)

From the bshift repository root directory:

```bash
asmtest
```




### The bshift language

A hello world example can be written like so:

```bshift
import io;

u64 main()
{
	io::print("Greetings!\n");

	return 0;
}
```

There are no header files like in C. When you import a module, such as *io*
from the standard library, the definitions are parsed from the module, it's
compiled to an object file, and bshift passes both object files to the linker
for linking.



### Built-in types in bshift

There are only a few built-in datatypes so far:

| Type | Size in memory | Description
|------|----------------|-------------
| bool | 1 byte         | Can be true or false
| u8   | 1 byte         | Unsigned 8-bit integer
| u64  | 8 bytes        | Unsigned 64-bit integer

You can also have arrays of these primitives and pointers to these primitives.
A pointer is always a u64 under the hood.



### Type inference

There is also an *auto* keyword which attempts to do some type inference:

```bshift
import io;

u64 increment(u64 x)
{
    return x + 1;
}

u64 main()
{
	auto y = increment(6);

	io::print("%u\n", y);

	return 0;
}
```



### Structs

Structs can be made up of primitives and/or other structs. They can also have
member methods:

```bshift
import io;

struct point
{
    u64 x;
    u64 y;
}

bool point::equals(point* other)
{
    return this.x == other.x && this.y == other.y;
}

u64 main()
{
    point p1;
    p1.x = 3;
    p1.y = 7;

    point p2;
    p2.x = 4;
    p2.y = 9;

	if (p1.equals(&p2))
	{
		io::print("p1 equals p2\n");
	}
	else
	{
		io::print("p1 does not equal p2\n");
	}

	return 0;
}
```



