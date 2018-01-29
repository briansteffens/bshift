# bshift

[![Build Status](https://travis-ci.org/briansteffens/bshift.svg?branch=master)](https://travis-ci.org/briansteffens/bshift)

```c
import io;

u64 factorial(u64 n)
{
    if (n == 0)
    {
        return 1;
    }

    return n * factorial(n - 1);
}

u64 main()
{
    io::print("%u\n", factorial(5));

    return 0;
}
```

This is a toy compiler for a C-style language with a few other features:

- Structs can have methods, including optional constructors and destructors
- Function overloading based on argument types
- Templates
- No header files

These come with some drawbacks, including:

- Symbol name mangling
- All the other drawbacks

For an example of a bshift program, take a look at
[show](https://github.com/briansteffens/show/blob/master/main.bs).

*Hint: don't use this for anything, it's super unstable.*




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

* [btest](https://github.com/briansteffens/btest)

From the bshift repository root directory:

```bash
btest
```




# The bshift language

A hello world example can be written like so:

```c
import io;

u64 main()
{
    io::print("Greetings!\n");

    return 0;
}
```




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

There is also an *auto* keyword (thanks @eatonphil) which attempts to do some
type inference:

```c
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



### Import

A module can be imported into another module using the import keyword:

```c
import io;

u64 main()
{
    io::print("hello\n");

    return 0;
}
```

In the above example, the compiler will look for an `io.bs` file in the
following locations, in order:

- ./io.bs
- ./lib/io.bs
- /usr/local/lib/bshift/io.bs

If it finds a match, any exported functions/structs will become available
by using the module name and the scope operator. So `io::print` refers to a
function called *print* which is exported by the module *io*.





### Unqualified imports

Specific functions or structs can be imported without requiring the module
scope operator by naming them directly:

```c
import print from io;

u64 main()
{
    print("hello\n");

    return 0;
}
```

You can also import multiple functions or structs from a module:

```c
import length, reverse from cstring;
```




### Export

By default, all definitions in a module are private to that module. In order
to make them available to other modules, they have to be exported:

```c
// example.bs

export u64 increment(u64 a)
{
    return a + 1;
}

export struct point
{
    u64 x;
    u64 y;
}

void not_exported()
{
}
```

The above module can be imported by another module:

```c
import io;
import example;

u64 main()
{
    example::point p;
    p.x = 3;

    u64 a = example::increment(p.x);

    io::print("%u\n", a);

    return 0;
}
```

The exported struct `point` and the exported function `increment` are available
to the importing module. However, trying to access the `not_exported` function
would result in an error.

When run, the output will be:

```
4
```





### Structs

Structs can be made up of primitives and/or other structs. They can also have
member methods:

```c
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




### Constructors

Structs can have constructors, which are used to initialize struct instances.
A constructor is defined by making a struct method called *construct* which
returns void:

```c
import io;

struct point
{
    u64 x;
    u64 y;
}

void point::construct(u64 x, u64 y)
{
    this.x = x;
    this.y = y;
}

u64 main()
{
    point p(3, 7);

    io::print("%u, %u\n", p.x, p.y);

    return 0;
}
```

When run, the output will be:

```
3, 7
```




### Destructors

Structs can have destructors, which are called automatically when they leave
scope. A destructor is defined by making a struct method called *destruct*
which takes no arguments and returns void:

```c
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

```c
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




### Function overloading

Functions can be defined which have the same name but differ based on the types
of their arguments:

```c
import io;

void show(u64 x)
{
    io::print("%u\n", x);
}

void show(u8* s)
{
    io::print("%s\n", s);
}

u64 main()
{
    show(3);
    show("hello");

    return 0;
}
```

When run, the output will be:

```
3
hello
```




### Method overloading

Struct methods can also be overloaded. One example use-case is to provide
multiple ways of initializing a struct instance:

```c
import io;

struct point
{
    u64 x;
    u64 y;
}

// Initialize a point with the given values
void point::construct(u64 x, u64 y)
{
    this.x = x;
    this.y = y;
}

// Initialize a point with default values
void point::construct()
{
    this.construct(0, 0);
}

u64 main()
{
    point p1();
    io::print("%u, %u\n", p1.x, p1.y);

    point p2(3, 7);
    io::print("%u, %u\n", p2.x, p2.y);
}
```

When run, the output will be:

```
0, 0
3, 7
```




### Function templates

Generic programming of functions is possible by using templates. After a
function name, put type parameters in angled brackets. When the function is
called with a new list of concrete types, a new version of the template will
be rendered:

```c
import io;

T max<T>(T a, T b)
{
    if (a > b)
    {
        return a;
    }

    return b;
}

u64 main()
{
    io::print("%u\n", max<u64>(3, 7));

    return 0;
}
```

When run, the output will be:

```
7
```

When `max<u64>(3, 7)` is called the first time, the compiler will notice that
the template has not been called yet with T set to u64. So it will convert the
following:

```c
T max<T>(T a, T b)
{
    if (a > b)
    {
        return a;
    }

    return b;
}
```

To this:

```c
u64 max(u64 a, u64 b)
{
    if (a > b)
    {
        return a;
    }

    return b;
}
```




### Struct templates

Generic structs are also possible using a similar syntax:

```c
import io;

struct container<T>
{
    T a;
    T b;
}

u64 main()
{
    container<u64> c;

    c.a = 3;
    c.b = 7;

    io::print("%u, %u\n", c.a, c.b);

    return 0;
}
```

When run, the output will be:

```
3, 7
```

When an instance of `container<u64>` is created, the compiler will convert the
following template:

```c
struct container<T>
{
    T a;
    T b;
}
```

To:

```c
struct container
{
    u64 a;
    u64 b;
}
```






### Sizeof

The `sizeof` built-in can be used to get the size (in bytes) of a type.
Examples:

```c
struct point
{
    u64 x;
    u64 y;
}

u64 main()
{
    u64 result0 = sizeof(u64);      // Returns 8
    u64 result1 = sizeof(point);    // Returns 16
    u64 result2 = sizeof(point*);   // Returns 8
    u64 result3 = sizeof(u8);       // Returns 1
    u64 result4 = sizeof(u8*);      // Returns 8

    return 0;
}
```







### Variadic functions

Functions can accept an arbitrary number of arguments, with a couple of
limitations:

1. Variadic arguments must come after any fixed positional arguments.
2. Variadic arguments are always type u64.

To make a function variadic, use the `...` operator:

```c
u64 sum(u64 total, ...)
{
    u64 ret = 0;

    for (u64 i = 0; i < total; i++)
    {
        ret = ret + variadic(i);
    }

    return ret;
}
```

This function can be called like this:

```c
    u64 result1 = sum(1, 15);         // Returns 15
    u64 result2 = sum(3, 50, 20, 80); // Returns 150
    u64 result3 = sum(0);             // Returns 0
```

You can access a variadic argument by position using the `variadic` built-in.
The `variadic` built-in takes a u64 index and returns the value of the
variadic argument at that index.

To retreive the first variadic argument, use `variadic(0)`. To retreive the
third, use `variadic(4)`.

*Notice that the total number of variadic arguments provided is not inherently
available. Variadic functions must provide some other way of communicating
this information, like the parameter `total` above.*







### System calls

You can make system calls using the built-in `syscall` function. It takes a
variable number of arguments. Each argument must be of the type `u64`. Only
the first argument is required. The position of the argument determines which
register the data will be copied into before the `syscall` instruction:

| Argument position | Target register |
|-------------------|-----------------|
| 0                 | rax             |
| 1                 | rdi             |
| 2                 | rsi             |
| 3                 | rdx             |
| 4                 | r10             |
| 5                 | r8              |
| 6                 | r9              |

So to make an `exit` system call with a status code of `0`:

```c
    syscall(60, 0);
```

This will result in the following approximate assembly:

```asm
    mov rax, 60
    mov rdi, 0
    syscall
```

The `syscall` function returns whatever value is left in `rax` after the
system call completes.









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
    io::print("Integer %u\n", 7);

    return 0;
}
```

Output:

```
Greetings!
Integer 7
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

Output:

```
Length: 16
```



### bool compare(u8* source, u8* destination)

Compares the contents of two C-style strings, returning true if they are
identical and false otherwise.

```c
import io;
import cstring;

u64 main()
{
    if (cstring::compare("these match", "these match"))
    {
        io::print("first is a match\n");
    }

    if (cstring::compare("not these", "though"))
    {
        io::print("this won't be written\n");
    }

    return 0;
}
```

Output:

```
first is a match
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

Output:

```
Converted to: 123
```





### u64 from_u64(u64 source, u8* destination)

Converts an integer to an ASCII C-string. *destination* must have enough bytes
to contain the value in *source*. Returns the number of digits written, not
including the null termination character.

```c
import io;
import cstring;

u64 main()
{
    u8[16] str;

    u64 written = cstring::from_u64(123, &str);

    io::print("digits written: %u\n", written);
    io::print(&str);
    io::print("\n");

    return 0;
}
```

Output:

```
digits written: 3
123
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

Output:

```
hello
```




## heap

The *heap* module implements a very basic memory manager.



### u8* allocate(u64 bytes)

Allocate a new buffer of size *bytes*. Returns the address to the beginning of
the new buffer, or 0 if the request couldn't be fulfilled.




### u8* reallocate(u8* ptr, u64 bytes)

Resize an existing buffer. Involves a linear time copy operation if the buffer
has to be moved to accommodate the size change. Returns the address to the
beginning of the buffer, which may or may not be the same as *ptr*.



### void free(u8* ptr)

Free a buffer previously allocated with *heap::allocate*, marking that region
of memory available for reuse by further allocation requests.




```c
import io;
import memory;
import heap;

u64 main()
{
    u8* str = heap::allocate(5);

    memory::copy("Greet", str, 5);

    io::print(str);
    io::print("\n");

    str = heap::reallocate(str, 10);

    memory::copy("ings!", str + 5, 5);

    io::print(str);
    io::print("\n");

    heap::free(str);

    return 0;
}
```

Output:

```
Greet
Greetings!
```







## math

The *math* module contains common math functions.



### T math::max<T>(T a, T b)

Returns the larger of the two given values.

```c
import io;
import math;

u64 main()
{
    auto r = math::max<u64>(10, 25);

    io::print("%u\n", r);
}
```

Output:
```
25
```




### T math::min<T>(T a, T b)

Returns the smaller of the two given values.

```c
import io;
import math;

u64 main()
{
    auto r = math::min<u64>(5, 3);

    io::print("%u\n", r);
}
```

Output:
```
3
```




### T math::align<T>(T value, T alignment)

Aligns `value` to the nearest larger multiple of `alignment`. For example,
`align(13, 4) = 16` and `align(12, 4) = 12`.

```c
import math;

u64 main()
{
    auto r = math::align<u64>(13, 4);

    io::print("%u\n", r);
}
```

Output:
```
16
```





## vector

The *vector* module contains an implementation of a dynamically-sized array.

Here's an example which creates a new vector of u64 integers, adds two
elements, then prints those elements out as well as the total length of the
vector:

```c
import io;
import vector from vector;

u64 main()
{
    vector<u64> v;

    v.add(3);
    v.add(7);

    io::print("%u\n", v.get(0));
    io::print("%u\n", v.get(1));
    io::print("%u\n", v.length);

    return 0;
}
```

When run, the output will be:

```
3
7
2
```






# Notes

Some notes on various peculiarities of the compiler:

- Compiles straight to Intel-style x86-64 assembly. Adding an LLVM mode would
  be really cool but there are no immediate plans.
- No 32-bit support.
- bshift programs don't link to libc by default. bshift has its own interface
  to syscalls and its own (extremely naive) memory manager.
