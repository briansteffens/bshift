import std.stdio;
import std.format;
import std.typecons;
import std.file;
import std.array;
import std.process;
import std.conv;
import core.sys.posix.stdlib;

import globals;
import ast;
import lexer;
import generator;
import grammar;
import terminal;

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

        if (arg == "-f")
        {
            force = true;
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
        if (result.changed || force)
        {
            asmFiles ~= result.asmFile;
        }
    }

    try
    {
        assemble(assembler, asmFiles);
    }
    catch (Exception ex)
    {
        writeln(ex);
        return 2;
    }

    string[] objectFiles;
    foreach (result; results)
    {
        objectFiles ~= result.objectFile;
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

void assemble(Assembler assembler, string[] sources)
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
    }
}

class CompileResult
{
    Module mod;
    string asmFile;
    string objectFile;
    bool changed;

    this(Module mod, string asmFile, string objectFile, bool changed)
    {
        this.mod = mod;
        this.asmFile = asmFile;
        this.objectFile = objectFile;
        this.changed = changed;
    }
}

string padRight(string source, ulong maxLength)
{
    while (source.length < maxLength)
    {
        source ~= " ";
    }

    return source;
}

void printSyntaxError(SyntaxError e)
{
    writeln("\n" ~ colorRed("error: ") ~ e.originalMessage);

    // Filename
    writeln(format(" %s %s:%d:%d", colorBlue("-->"), e.line.file,
            e.line.number, e.lineOffset));

    // Source code
    auto center = e.line;
    auto current = center;
    Line[] lines = [];

    // Rewind up to 2 lines
    for (int i = 0; i < 2 && current.previous !is null; i++)
    {
        current = current.previous;
    }

    // Take up to 5 lines
    for (int i = 0; i < 5 && current.next !is null; i++)
    {
        lines ~= current;
        current = current.next;
    }

    // Padding for line numbers
    auto padding = to!string(center.number).length;

    // Output source code
    foreach (line; lines)
    {
        string left = "";

        if (line == center)
        {
            left = to!string(line.number);
        }

        left = padRight(left, padding);

        writeln(format("%s | %s", left, line.source));

        if (line == center)
        {
            writeln(padRight("", padding + 2 + e.lineOffset),
                    colorRed("^ somewhere around here"));
        }
    }
    writeln();
}

// Compile a module
CompileResult compile(string sourceFilename)
{
    auto asmFilename = replace(sourceFilename, ".bs", ".asm");
    auto objectFilename = replace(sourceFilename, ".bs", ".o");

    auto sourceModified = timeLastModified(sourceFilename);

    bool changed = !exists(asmFilename) ||
                   sourceModified > timeLastModified(asmFilename);

    // Source code
    auto source = readText(sourceFilename);

    if (verbose)
    {
        writeln("bshift source code --------------------------------------\n");
        writeln(source);
    }

    // Lexer
    auto tokens = lex(source, sourceFilename);

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
    Module mod;

    try
    {
        mod = parse(moduleName, tokens);
    }
    catch (SyntaxError e)
    {
        printSyntaxError(e);
        exit(7);
    }

    if (changed || force)
    {
        if (verbose)
        {
            writeln("bshift ast ------------------------------------------\n");
            writeln(mod);
        }

        // Generator
        auto output = generate(mod);

        // Write assembly output
        auto contents = renderAsmFile(output);

        if (verbose)
        {
            writeln("bshift output ---------------------------------------\n");
            writeln(contents);
        }

        auto bytes = cast(ubyte[])contents;

        auto file = File(asmFilename, "w");
        file.rawWrite(bytes);
    }

    return new CompileResult(mod, asmFilename, objectFilename, changed);
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
