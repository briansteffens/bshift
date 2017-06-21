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




# The bshift language

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
| u8   | 1 byte         | Unsigned 8-bit integer, often used as an ASCII character
| u64  | 8 bytes        | Unsigned 64-bit integer
| void | N/A            | Placeholder keyword for functions that don't return a value

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



### Destructors

Structs can have destructors, which are called automatically when they leave
scope. A destructor is defined by making a member function called *destruct*
which takes no arguments and returns void:

```bshift
import io;

struct val
{
    u64 x;
}

void val::destruct()
{
    io::print("destruct called\n");
}

u64 main()
{
    val v;

    io::print("main() body\n");

    return 0;
}
```

When run, the output will be:

```
main() body
destruct called
```





### Defer

Statements can be deferred to the end of the current scope, similar to Go, to
help with cleaning up heap-allocated memory, open file handles, etc:

```bshift
import io;

u64 main()
{
    defer io::print("deferred\n");

    io::print("not deferred\n");

    return 0;
}
```

The output will be:

```
not deferred
deferred
```






# Standard library

The standard library is a collection of modules located in `lib/`.





## io

The *io* module contains helper functions for input/output.

### u64 io::print(u8* fmt, ...)

Prints text to standard output, with some formatting options.

```c
import io

u64 main()
{
    io::print("Hello\n");
    io::print("An integer %u\n", 7);

    return 0;
}
```






## cstring

The *cstring* module contains functions for dealing with C-style
null-terminated ASCII strings.



### u64 cstring::length(u8* str)

Returns the length of the given buffer by traversing it until the
null-termination byte 0 is found.

```c
import io;
import cstring;

u64 main()
{
    u8* str = "here is a string";

    io::print("Length: %u\n", cstring::length(str));

    return 0;
}
```



### bool to_u64(u8* source, u64* destination)

Converts digits from a C-style string into a u64 integer. Returns true if the
operation was successful or false if it failed.

```c
import io;
import cstring;

u64 main()
{
    u8* str = "123";

    u64 converted = 0;

    if (cstring::to_u64(str, &converted) == false)
    {
        io::print("Failed to convert string to u64\n");
        return 1;
    }

    io::print("Converted to: %u\n", converted);

    return 0;
}
```





## memory

The *memory* module contains helper functions for working with raw buffers of
memory on a byte-level.

### void copy(u8* source, u8* destination, u64 bytes)

Copies *bytes* bytes from *source* to *destination*.

```c
import io;
import memory;

u64 main()
{
    u8* src = "hello\n";
    u8* dst = "erased";

    memory::copy(src, dst, 6);

    io::print(dst);

    return 0;
}
```


