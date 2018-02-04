import std.stdio;
import std.format;
import std.typecons;
import std.file;
import std.array;
import std.process;
import std.conv;
import std.algorithm.comparison;
import core.sys.posix.stdlib;

import globals;
import debugging;
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
    auto results = recursiveCompile(imported, sourceFilename, true);

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
    for (int i = 0; i < sources.length; i++)
    {
        string source = sources[i];
        bool isFirst = i == 0;

        string command;

        switch (assembler)
        {
            case Assembler.BASM:
                string _verbose = verbose && isFirst ? "-v" : "";

                command = format("basm %s %s", _verbose, source);
                break;

            case Assembler.NASM:
                version (OSX)
                {
                    command = format("nasm -f macho64 %s", source);
                }
                else
                {
                    command = format("nasm -f elf64 %s", source);
                }
                break;

            default:
                throw new Exception(
                        format("Unrecognized assembler %s", assembler));
        }

        auto asmOut = executeShell(command);

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

// Compile a module
CompileResult compile(string sourceFilename, bool isPrimarySource)
{
    bool _verbose = verbose && isPrimarySource;

    auto asmFilename = replace(sourceFilename, ".bs", ".asm");
    auto objectFilename = replace(sourceFilename, ".bs", ".o");

    auto sourceModified = timeLastModified(sourceFilename);

    bool changed = !exists(asmFilename) ||
                   sourceModified > timeLastModified(asmFilename);

    // Source code
    auto source = readText(sourceFilename);

    if (_verbose)
    {
        writeln("bshift source code --------------------------------------\n");
        writeln(source);
    }

    // Lexer
    SourceFile file;
    try
    {
        file = lex(sourceFilename, source);
    }
    catch (LexerError e)
    {
        printSyntaxError(e.msg, e.location);
        exit(6);
    }

    Token[] tokens = file.tokens;

    if (_verbose)
    {
        debugTokens(file.tokens);
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
        printSyntaxError(e.msg, e.token.location);
        exit(7);
    }

    if (changed || force)
    {
        if (_verbose)
        {
            writeln("bshift ast ------------------------------------------\n");
            writeln(mod);
        }

        // Generator
        auto output = generate(mod);

        // Write assembly output
        auto contents = renderAsmFile(output);

        if (_verbose)
        {
            writeln("bshift output ---------------------------------------\n");
            writeln(contents);
        }

        auto bytes = cast(ubyte[])contents;

        auto outputFile = File(asmFilename, "w");
        outputFile.rawWrite(bytes);
    }

    return new CompileResult(mod, asmFilename, objectFilename, changed);
}

// Compile a module and any imported modules
CompileResult[] recursiveCompile(ref Import[] imported, string sourceFilename,
        bool isPrimarySource)
{
    CompileResult[] ret;

    auto result = compile(sourceFilename, isPrimarySource);
    ret ~= result;

    // Turn off verbose mode for imports, it's a ton of extra noise for little
    // benefit. Restore it afterward.
    bool verboseOld = verbose;
    verbose = false;

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

        ret ~= recursiveCompile(imported, imp.filename, false);

        imported ~= imp;
    }

    verbose = verboseOld;

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
