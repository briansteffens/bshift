import std.stdio;
import std.format;
import std.typecons;
import std.file;
import std.array;

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
    auto sourceFilename = args[1];
    auto source = readText(sourceFilename);

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
    auto moduleName = replace(sourceFilename, ".bs", "");
    auto mod = parse(moduleName, tokens);

    writeln("bshift ast --------------------------------------------------\n");
    writeln(mod);

    // Generator
    auto output = generate(mod);

    // Write assembly output
    auto contents = renderAsmFile(output);

    writeln("bshift output -----------------------------------------------\n");
    writeln(contents);

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
