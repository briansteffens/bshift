import std.stdio;
import std.format;
import std.typecons;
import std.file;
import std.array;
import std.process;

import globals;
import ast;
import lexer;
import parser;
import generator;

enum Assembler
{
    BASM,
    NASM,
}

int main(string[] args)
{
    string sourceFilename = null;
    auto assembler = Assembler.BASM;

    foreach (arg; args[1..$])
    {
        if (arg == "-v" || arg == "--verbose")
        {
            verbose = true;
            continue;
        }

        if (arg == "--nasm")
        {
            assembler = Assembler.NASM;
            continue;
        }

        sourceFilename = arg;
    }

    if (sourceFilename is null)
    {
        writeln("Usage: bin/bshift [--basm | --nasm] [-v, --verbose] <filename>");
        return 1;
    }

    // [.bs] -> [.asm]
    Import[] imported;
    auto results = recursiveCompile(imported, sourceFilename);

    // [.asm] -> [.o]
    string[] asmFiles;
    foreach (result; results)
    {
        asmFiles ~= result.asmFile;
    }

    string[] objectFiles;
    try
    {
        objectFiles = assemble(assembler, asmFiles);
    }
    catch (Exception ex)
    {
        writeln(ex);
        return 2;
    }

    // [.o] -> a.out
    try
    {
        link(objectFiles);
    }
    catch (Exception ex)
    {
        writeln(ex);
        return 3;
    }

    return 0;
}

void link(string[] objectFiles)
{
    auto cmd = format("ld -e _start %s", objectFiles.join(" "));
    auto res = executeShell(cmd);

    if (res.status != 0)
    {
        writeln(res.output);

        throw new Exception(format("Failed to link using '%s'", cmd));
    }
}

string[] assemble(Assembler assembler, string[] sources)
{
    string assemblerTemplate = null;

    switch (assembler)
    {
        case Assembler.BASM:
            if (verbose)
            {
                assemblerTemplate = "basm -v %s";
            }
            else
            {
                assemblerTemplate = "basm %s";
            }

            break;

        case Assembler.NASM:
            version (OSX)
            {
                assemblerTemplate = "nasm -f macho64 %s";
            }
            else
            {
                assemblerTemplate = "nasm -f elf64 %s";
            }
            break;

        default:
            throw new Exception(
                    format("Unrecognized assembler %s", assembler));
    }

    string[] ret;

    foreach (source; sources)
    {
        auto asmOut = executeShell(format(assemblerTemplate, source));

        if (verbose)
        {
            writeln(asmOut.output);
        }

        if (asmOut.status != 0)
        {
            throw new Exception(format("Failed to assemble %s: %d", source,
                                       asmOut.status));
        }

        ret ~= source[0..$-4] ~ ".o";
    }

    return ret;
}

class CompileResult
{
    Module mod;
    string asmFile;

    this(Module mod, string asmFile)
    {
        this.mod = mod;
        this.asmFile = asmFile;
    }
}

// Compile a module
CompileResult compile(string sourceFilename)
{
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
    auto moduleName = replace(sourceFilename, ".bs", "").split("/")[$-1];
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

    return new CompileResult(mod, outputFilename);
}

// Compile a module and any imported modules
CompileResult[] recursiveCompile(ref Import[] imported, string sourceFilename)
{
    CompileResult[] ret;

    auto result = compile(sourceFilename);
    ret ~= result;

    foreach (imp; result.mod.imports)
    {
        bool found = false;
        foreach (i; imported)
        {
            if (i.filename == imp.filename)
            {
                found = true;
                break;
            }
        }

        if (found)
        {
            continue;
        }

        ret ~= recursiveCompile(imported, imp.filename);

        imported ~= imp;
    }

    return ret;
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
