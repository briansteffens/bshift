import std.stdio;
import std.format;
import std.typecons;
import std.file;

import ast;
import lexer;
import parser;
import generator;

void main(string[] args)
{
    if (args.length != 2)
    {
        writeln("Usage: ./main <filename>");
        return;
    }

    // Source code
    auto source = readText(args[1]);

    writeln("bshift source code ------------------------------------------\n");
    writeln(source);

    // Lexer
    auto tokens = lex(source);

    writeln("bshift tokens -----------------------------------------------\n");
    for (int i = 0; i < tokens.length; i++)
    {
        writeln(tokens[i]);
    }

    writeln();

    // Parser
    auto mod = parse(tokens);

    writeln("bshift ast --------------------------------------------------\n");
    writeln(mod);

    // Generator
    auto output = generate(mod);

    // Write nasm output
    auto contents = renderAsmFile(output);

    writeln("bshift output -----------------------------------------------\n");
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
