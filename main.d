import std.stdio;
import std.format;
import std.typecons;
//import std.file;

import ast;
import lexer;
import parser;
import generator;

void main()
{
    // Source code
    auto src = "ulong main ( ) { ulong x ; x = 3 + 5 ; return x + 1 ; } ";

    writeln(src);
    writeln();

    // Lexer
    auto tokens = lex(src);

    for (int i = 0; i < tokens.length; i++)
    {
        writeln(tokens[i]);
    }

    writeln();

    // Parser
    auto mod = parse(tokens);

    writeln();
    writeln(mod);

    // Generator
    auto output = generate(mod);

    // Write nasm output
    auto contents = renderAsmFile(output);
    writeln(contents);

    auto bytes = cast(ubyte[])contents;

    auto file = File("output.asm", "w");
    file.rawWrite(bytes);
}

string renderAsmFile(string[] output)
{
    string ret = "";

    for (auto i = 0; i < output.length; i++)
    {
        ret ~= output[i] ~ "\n";
    }

    return ret;
}
