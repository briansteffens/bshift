import std.stdio;
import std.format;
import std.typecons;
import std.file;
import std.array;

import globals;
import ast;
import lexer;
import parser;
import generator;

void main(string[] args)
{
    string sourceFilename = null;

    foreach (arg; args[1..$])
    {
        if (arg == "-v")
        {
            verbose = true;
        }
        else
        {
            sourceFilename = arg;
        }
    }

    if (sourceFilename is null)
    {
        writeln("Usage: ./main <filename>");
        return;
    }

    // Source code
    auto source = readText(sourceFilename);

    if (verbose)
    {
        writeln("bshift source code --------------------------------------\n");
        writeln(source);
    }

    // Lexer
    auto tokens = lex(source);

    if (verbose)
    {
        writeln("bshift tokens -------------------------------------------\n");
        for (int i = 0; i < tokens.length; i++)
        {
            writeln(tokens[i]);
        }

        writeln();
    }

    // Parser
    auto moduleName = replace(sourceFilename, ".bs", "");
    auto mod = parse(moduleName, tokens);

    if (verbose)
    {
        writeln("bshift ast ----------------------------------------------\n");
        writeln(mod);
    }

    // Generator
    auto output = generate(mod);

    // Write assembly output
    auto contents = renderAsmFile(output);

    if (verbose)
    {
        writeln("bshift output -------------------------------------------\n");
        writeln(contents);
    }

    auto bytes = cast(ubyte[])contents;

    auto outputFilename = replace(sourceFilename, ".bs", ".asm");
    auto file = File(outputFilename, "w");
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
