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
    /*
    auto state = new GeneratorState(null);
    RegisterMove[] moves;

    moves ~= new RegisterMove(Register.RAX, Register.RDI);
    moves ~= new RegisterMove(Register.RDI, Register.RSI);

    moves ~= new RegisterMove(Register.R8, Register.R9);
    moves ~= new RegisterMove(Register.R9, Register.R8);

    moves ~= new RegisterMove(Register.R10, Register.R11);
    moves ~= new RegisterMove(Register.R11, Register.R12);
    moves ~= new RegisterMove(Register.R12, Register.R10);

    state.temps ~= new Local(Type.ULong, "a");
    state.temps[$-1].location = Location.Register;
    state.temps[$-1].register = Register.RAX;

    state.temps ~= new Local(Type.ULong, "b");
    state.temps[$-1].location = Location.Register;
    state.temps[$-1].register = Register.RDI;

    state.temps ~= new Local(Type.ULong, "c");
    state.temps[$-1].location = Location.Register;
    state.temps[$-1].register = Register.RSI;

    state.temps ~= new Local(Type.ULong, "d");
    state.temps[$-1].location = Location.Register;
    state.temps[$-1].register = Register.R8;

    state.temps ~= new Local(Type.ULong, "e");
    state.temps[$-1].location = Location.Register;
    state.temps[$-1].register = Register.R9;

    state.temps ~= new Local(Type.ULong, "f");
    state.temps[$-1].location = Location.Register;
    state.temps[$-1].register = Register.R10;

    state.temps ~= new Local(Type.ULong, "g");
    state.temps[$-1].location = Location.Register;
    state.temps[$-1].register = Register.R11;

    state.temps ~= new Local(Type.ULong, "h");
    state.temps[$-1].location = Location.Register;
    state.temps[$-1].register = Register.R12;

    shuffleRegisters(state, moves);

    foreach (x; state.output)
    {
        writeln(x);
    }

    return;
    */

    if (args.length != 2)
    {
        writeln("Usage: ./main <filename>");
        return;
    }

    // Source code
    auto source = readText(args[1]);

    writeln(source);
    writeln();

    // Lexer
    auto tokens = lex(source);

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
