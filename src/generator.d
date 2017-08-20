import std.stdio;
import std.format;
import std.conv;
import std.array;
import std.algorithm;

import globals;
import ast;

enum Register
{
    RAX,
    RBX,
    RCX,
    RDX,
    RSI,
    RDI,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,

    AL,
    BL,
    CL,
    DL,
    SIL,
    DIL,
    R8B,
    R9B,
    R10B,
    R11B,
    R12B,
    R13B,
    R14B,
    R15B
}

enum OpSize
{
    Byte,
    Word,
    Dword,
    Qword,
}

bool isRegister64(Register r)
{
    return r == Register.RAX || r == Register.RBX || r == Register.RCX ||
           r == Register.RDX || r == Register.RSI || r == Register.RDI ||
           r == Register.R8  || r == Register.R9  || r == Register.R10 ||
           r == Register.R11 || r == Register.R12 || r == Register.R13 ||
           r == Register.R14 || r == Register.R15;
}

bool isRegisterLow8(Register r)
{
    return r == Register.AL   || r == Register.BL   || r == Register.CL   ||
           r == Register.DL   || r == Register.SIL  || r == Register.DIL  ||
           r == Register.R8B  || r == Register.R9B  || r == Register.R10B ||
           r == Register.R11B || r == Register.R12B || r == Register.R13B ||
           r == Register.R14B || r == Register.R15B;
}

OpSize registerSize(Register r)
{
    if (isRegister64(r))
    {
        return OpSize.Qword;
    }

    if (isRegisterLow8(r))
    {
        return OpSize.Byte;
    }

    throw new Exception(format("Unrecognized register: %s", r));
}

int opSizeBytes(OpSize o)
{
    switch (o)
    {
        case OpSize.Byte:
            return 1;
        case OpSize.Word:
            return 2;
        case OpSize.Dword:
            return 4;
        case OpSize.Qword:
            return 8;
        default:
            throw new Exception(format("Unrecognized OpSize: %s", o));
    }
}

Register lowByteRegister(Register full)
{
    switch (full)
    {
        case Register.RAX:
            return Register.AL;
        case Register.RBX:
            return Register.BL;
        case Register.RCX:
            return Register.CL;
        case Register.RDX:
            return Register.DL;
        case Register.RSI:
            return Register.SIL;
        case Register.RDI:
            return Register.DIL;
        case Register.R8:
            return Register.R8B;
        case Register.R9:
            return Register.R9B;
        case Register.R10:
            return Register.R10B;
        case Register.R11:
            return Register.R11B;
        case Register.R12:
            return Register.R12B;
        case Register.R13:
            return Register.R13B;
        case Register.R14:
            return Register.R14B;
        case Register.R15:
            return Register.R15B;
        default:
            throw new Exception(
                    format("Can't find the low byte for %s", full));
    }
}

string lowByte(Register full)
{
    return to!string(lowByteRegister(full));
}

Register convertRegisterSize(Register r, OpSize size)
{
    if (registerSize(r) == size)
    {
        return r;
    }

    if (size == OpSize.Byte)
    {
        return lowByteRegister(r);
    }

    throw new Exception(format("Can't convert %s to size %s", r, size));
}

OpSize primitiveToOpSize(Primitive t)
{
    switch (t)
    {
        case Primitive.Bool:
            return OpSize.Byte;
        case Primitive.U64:
            return OpSize.Qword;
        case Primitive.U8:
            return OpSize.Byte;
        default:
            throw new Exception(format("Unknown type %s", t));
    }
}

OpSize typeToOpSize(Type t)
{
    if (t.isStruct() || t.pointer)
    {
        return OpSize.Qword;
    }

    if (t.isPrimitive())
    {
        return primitiveToOpSize((cast(PrimitiveType)t).primitive);
    }

    throw new Exception(format("Can't calculate size of %s", t));
}

int typeSize(Type t)
{
    if (t.elements !is null)
    {
        if (!t.isConcrete())
        {
            throw new Exception("Cannot calculate size of non-concrete type");
        }

        auto arraySize = cast(U64Literal)t.elements;
        if (arraySize is null)
        {
            throw new Exception("Bad type elements value");
        }

        return cast(int)arraySize.value * t.baseTypeSize();
    }

    if (t.pointer)
    {
        return 8;
    }

    return t.baseTypeSize();
}

// Align n to the next multiple of multiplier (alignNext(13, 8) = 16)
int alignNext(int n, int multiplier)
{
    int remainder = n % multiplier;

    if (remainder == 0)
    {
        return n;
    }

    return n + multiplier - remainder;
}

// A register or region of the stack where some data can be located
abstract class DataLocation
{
    int bytes;
}

// A literal value
class LiteralLocation : DataLocation
{
    Literal literal;

    this(Literal literal)
    {
        this.literal = literal;
        this.bytes = typeSize(literal.type);
    }
}

// A register where all or some of a variable is located
class RegisterLocation : DataLocation
{
    Register register;

    this(Register register)
    {
        this.register = register;
        this.bytes = opSizeBytes(registerSize(register));
    }
}

// A stack location where all or some of a variable is located
class StackLocation : DataLocation
{
    int offset;

    this(int offset, int bytes)
    {
        this.offset = offset;
        this.bytes = bytes;
    }
}

// Data stored in the global data section of the object file
class DataSectionLocation : DataLocation
{
    string name;

    this(string name)
    {
        this.name = name;
    }
}

class Local : Node
{
    string name;

    // Where this local's data is
    DataLocation[] data;

    this(Type type, string name)
    {
        this.type = type;
        this.name = name;
    }

    RegisterLocation[] registers()
    {
        RegisterLocation[] ret;

        foreach (location; this.data)
        {
            auto r = cast(RegisterLocation)location;

            if (r !is null)
            {
                ret ~= r;
            }
        }

        return ret;
    }

    bool inRegister()
    {
        if (this.data.length != 1)
        {
            return false;
        }

        auto location = cast(RegisterLocation)this.data[0];
        if (location is null)
        {
            return false;
        }

        return true;
    }

    // If this local occupies a single register only, return that register.
    // Otherwise, fail.
    Register expectSingleRegister()
    {
        if (!this.inRegister())
        {
            throw new Exception("Expected local to have a single register");
        }

        auto location = cast(RegisterLocation)this.data[0];
        if (location is null)
        {
            throw new Exception("Expected local to have a single register");
        }

        return location.register;
    }
}

bool isCallerPreserved(Register r)
{
    return r == Register.RAX ||
           r == Register.RCX ||
           r == Register.RDX ||
           r == Register.RSI ||
           r == Register.RDI ||
           r == Register.R8  ||
           r == Register.R9  ||
           r == Register.R10 ||
           r == Register.R11;
}

string renderStringLiteralName(ulong index)
{
    return format("string_literal_%d", index);
}

class GeneratorState
{
    Module mod;

    string[] output;

    Local[] locals;
    Local[] temps;

    Register[] reserved;

    string[] labels;
    string[] externs;
    string[] stringLiterals;

    int nextTempIndex = 0;

    this(Module mod)
    {
        this.mod = mod;
    }

    // Prevent register from being used for temps/locals until unreserved.
    void reserve(Register r)
    {
        this.reserved ~= r;
    }

    void unreserve(Register r)
    {
        Register[] newReserved;

        foreach (res; this.reserved)
        {
            if (res != r)
            {
                newReserved ~= res;
            }
        }

        this.reserved = newReserved;
    }

    void render(string line)
    {
        if (verbose)
        {
            writefln("RENDER: %s", line);
        }

        this.output ~= line;
    }

    string addStringLiteral(string value)
    {
        this.stringLiterals ~= value;
        return renderStringLiteralName(this.stringLiterals.length - 1);
    }

    void addExtern(string name)
    {
        foreach (ext; this.externs)
        {
            if (ext == name)
            {
                return;
            }
        }

        this.externs ~= name;
    }

    // Find a unique new label by adding numbers to the end of prefix
    string addLabel(string prefix)
    {
        int counter = 0;

        while (true)
        {
            string ret = format("%s%d", prefix, counter);

            bool found = false;
            for (int i = 0; i < this.labels.length; i++)
            {
                if (this.labels[i] == ret)
                {
                    found = true;
                    break;
                }
            }

            if (!found)
            {
                this.labels ~= ret;
                return ret;
            }

            counter++;
        }
    }

    Register[] callerPreservedRegistersInUse()
    {
        Register[] ret;

        void processList(Local[] locals)
        {
            foreach (local; locals)
            {
                foreach (r; local.registers())
                {
                    if (isCallerPreserved(r.register))
                    {
                        ret ~= r.register;
                    }
                }
            }
        }

        processList(this.locals);
        processList(this.temps);

        return ret;
    }

    void generatePush(Register r)
    {
        this.render(format("    push %s", r));
    }

    void generatePop(Register r)
    {
        this.render(format("    pop %s", r));
    }

    bool registerTaken(Register register)
    {
        bool processList(Local[] locals)
        {
            foreach (local; locals)
            {
                foreach (r; local.registers())
                {
                    if (r.register == register)
                    {
                        return true;
                    }
                }
            }

            return false;
        }

        if (processList(this.locals) || processList(this.temps))
        {
            return true;
        }

        foreach (res; this.reserved)
        {
            if (res == register)
            {
                return true;
            }
        }

        return false;
    }

    Register findFreeRegister()
    {
        Register ret = Register.RAX;

        while (registerTaken(ret))
        {
            if (ret == Register.R15)
            {
                throw new Exception("Out of registers!");
            }

            ret++;
        }

        return ret;
    }

    Register reserveNextFreeRegister()
    {
        auto ret = findFreeRegister();
        reserve(ret);
        return ret;
    }

    Local addTemp(Type type)
    {
        auto ret = new Local(type, format("temp%d", this.nextTempIndex++));
        ret.data = [new RegisterLocation(this.findFreeRegister())];

        this.temps ~= ret;

        return ret;
    }

    void freeTemp(Local temp)
    {
        Local[] newTemps;

        foreach (existing; this.temps)
        {
            if (existing != temp)
            {
                newTemps ~= existing;
            }
        }

        this.temps = newTemps;
    }

    Local findLocal(string name)
    {
        // Look in locals
        foreach (local; this.locals)
        {
            if (local.name == name)
            {
                return local;
            }
        }

        // Look in temps
        foreach (temp; this.temps)
        {
            if (temp.name == name)
            {
                return temp;
            }
        }

        // Look in globals
        foreach (global; this.mod.globals)
        {
            if (global.signature.name == name)
            {
                auto ret = new Local(global.signature.type,
                        global.signature.name);

                ret.data = [new DataSectionLocation(global.signature.name)];

                return ret;
            }
        }

        return null;
    }
}

string renderGlobal(GeneratorState state, Global global)
{
    string type = null;

    if (global.signature.type.pointer)
    {
        type = "dq";
    }

    auto primitiveType = cast(PrimitiveType)global.signature.type;
    if (type is null && primitiveType !is null)
    {
        switch (primitiveType.primitive)
        {
            case Primitive.U64:
                type = "dq";
                break;
            case Primitive.U8:
            case Primitive.Bool:
                type = "db";
                break;
            default:
                throw new Exception(format("Can't convert %s to asm data type",
                        primitiveType));
        }
    }

    if (type is null)
    {
        throw new Exception(format("Can't convert %s to asm data type",
                    primitiveType));
    }

    auto value = "0";

    if (global.value !is null)
    {
        value = renderNode(state, global.value);
    }

    return format("    %s: %s %s", global.signature.name, type, value);
}

string renderStringLiteral(GeneratorState state, ulong index)
{
    auto value = "0";

    if (state.stringLiterals[index] != "")
    {
        value = state.stringLiterals[index];

        value = value.replace("\\", "\\\\")
                     .replace("\n", "\\n")
                     .replace("\t", "\\t");

        value = format("\"%s\", 0", value);
    }

    return format("    %s: db %s", renderStringLiteralName(index), value);
}

string[] generate(Module mod)
{
    auto state = new GeneratorState(mod);

    // Render data section
    string[] data;

    foreach (global; mod.globals)
    {
        data ~= renderGlobal(state, global);
    }

    // Render text section
    state.render("section .text");

    // Generate functions
    foreach (func; mod.functions)
    {
        generateFunction(state, func);
    }

    // Bootstrap the main function if there is one
    if (mod.functionExists("main"))
    {
        auto mainFunc = mod.findFunction("main");

        state.render("global _start");
        state.render("_start:");
        state.render(format("    call %s", mainFunc.signature.name));
        state.render(format("    mov rdi, rax"));
        version (OSX)
        {
            state.render("    mov rax, 0x2000001");
        }
        else
        {
            state.render("    mov rax, 60");
        }
        state.render("    syscall");
    }

    // Render string literals
    for (ulong i = 0; i < state.stringLiterals.length; i++)
    {
        data ~= renderStringLiteral(state, i);
    }

    if (data.length > 0)
    {
        data = "section .data" ~ data;
    }
    else
    {
        version (OSX)
        {
            // Mac requires dummy data section with dummy value for some reason
            data ~= "section .data";
            data ~= "_dummy: db 0";
        }
    }

    // Render externs
    string[] externs;
    foreach (ext; state.externs)
    {
        externs ~= format("extern %s", ext);
    }

    state.output = data ~ externs ~ state.output;

    return state.output;
}

Register[] callRegisters = [
    Register.RDI,
    Register.RSI,
    Register.RDX,
    Register.RCX,
    Register.R8,
    Register.R9
];

Register[] syscallRegisters = [
    Register.RDI,
    Register.RSI,
    Register.RDX,
    Register.R10,
    Register.R8,
    Register.R9
];

string renderName(FunctionSignature sig)
{
    if (sig.name == "main")
    {
        return sig.name;
    }

    auto ret = sig.name;

    auto method = cast(MethodSignature)sig;
    if (method !is null)
    {
        ret = format("%s_%s", method.containerType, ret);
    }

    return format("%s_%s", sig.mod.name, ret);
}

void generateFunction(GeneratorState state, Function func)
{
    int stackOffset = 0;

    // Detect main function with argc and argv
    bool mainWithArgs = false;
    if (func.signature.name == "main" &&
        func.signature.parameters.length == 2 &&
        func.signature.parameters[0].type.compare(
            new PrimitiveType(Primitive.U64)) &&
        func.signature.parameters[1].type.compare(
            new PrimitiveType(Primitive.U64, true)))
    {
        mainWithArgs = true;

        auto argc = new Local(func.signature.parameters[0].type,
                              func.signature.parameters[0].name);

        argc.data = [new StackLocation(16, 8)];

        state.locals ~= argc;

        auto argv = new Local(func.signature.parameters[1].type,
                              func.signature.parameters[1].name);

        // TODO: better indication of unknown stack data size?
        argv.data = [new StackLocation(24, -1)];

        state.locals ~= argv;
    }

    // Function parameters need to be added to locals
    if (!mainWithArgs)
    {
        int consumed = 0;
        const int registerSpace = cast(int)callRegisters.length * 8;

        foreach (param; func.signature.parameters)
        {
            auto local = new Local(param.type, param.name);
            auto sizeLeft = alignNext(typeSize(param.type), 8);

            while (sizeLeft > 0)
            {
                // There are still registers to use
                if (consumed < registerSpace)
                {
                    auto index = 0;

                    if (consumed > 0)
                    {
                        index = consumed / 8;
                    }

                    local.data ~= new RegisterLocation(callRegisters[index]);

                    sizeLeft -= 8;
                    consumed += 8;

                    continue;
                }

                // Registers are used up: overflow to the stack
                local.data ~= new StackLocation(consumed - registerSpace + 16,
                        sizeLeft);
                consumed += sizeLeft;

                break;
            }

            state.locals ~= local;
        }
    }

    // Shift locals down after all registers if it's a variadic function
    if (func.signature.variadic)
    {
        stackOffset = cast(int)callRegisters.length * -8;
    }

    // Register locals
    auto declarations = func.block.declarations();
    foreach (decl; declarations)
    {
        auto existing = state.findLocal(decl.signature.name);

        if (existing !is null)
        {
            throw new Exception(format("Local %s already declared",
                    decl.signature.name));
        }

        auto local = new Local(decl.signature.type, decl.signature.name);

        auto size = 0;
        if (local.type.isConcrete())
        {
            size = typeSize(local.type);
        }
        else
        {
            // Dynamic arrays need a single pointer in prologue space, which
            // will later point to their actual data region, allocated higher
            // in the stack.
            size = 8;
        }

        stackOffset -= size;

        local.data = [new StackLocation(stackOffset, size)];

        state.locals ~= local;
    }

    // Function prologue TODO: add export/public keyword to control this
    state.render(format("global %s", renderName(func.signature)));
    state.render(format("%s:", renderName(func.signature)));
    state.render(format("    push rbp"));
    state.render(format("    mov rbp, rsp"));

    if (stackOffset < 0)
    {
        state.render(format("    sub rsp, %s", stackOffset * -1));
    }

    // Copy variadic parameters passed through registers into the stack
    if (func.signature.variadic)
    {
        for (int i = cast(int)func.signature.parameters.length;
             i < callRegisters.length; i++)
        {
            state.render(format("    mov %s, %s",
                    renderStackLocation(i * -8 - 8), callRegisters[i]));
        }
    }

    generateBlock(state, func.block);

    state.locals.length = 0;
    state.temps.length = 0;
}

string renderStackLocation(int offset)
{
    if (offset < 0)
    {
        return format("[rbp - %d]", offset * -1);
    }
    else
    {
        return format("[rbp + %d]", offset);
    }
}

void generateStatementBase(GeneratorState state, StatementBase st)
{
    auto ignoreReturn = cast(Statement)st;
    if (ignoreReturn !is null)
    {
        generateStatement(state, ignoreReturn);
        return;
    }

    auto localDeclaration = cast(LocalDeclaration)st;
    if (localDeclaration !is null)
    {
        generateLocalDeclaration(state, localDeclaration);
        return;
    }

    auto assignment = cast(Assignment)st;
    if (assignment !is null)
    {
        generateAssignment(state, assignment);
        return;
    }

    auto _return = cast(Return)st;
    if (_return !is null)
    {
        generateReturn(state, _return);
        return;
    }

    auto block = cast(Block)st;
    if (block !is null)
    {
        generateBlock(state, block);
        return;
    }

    auto _if = cast(If)st;
    if (_if !is null)
    {
        generateIf(state, _if);
        return;
    }

    auto _while = cast(While)st;
    if (_while !is null)
    {
        generateWhile(state, _while);
        return;
    }

    auto defer = cast(Defer)st;
    if (defer !is null)
    {
        // Skip defer
        return;
    }

    auto _break = cast(Break)st;
    if (_break !is null)
    {
        generateBreak(state, _break);
        return;
    }

    auto _continue = cast(Continue)st;
    if (_continue !is null)
    {
        generateContinue(state, _continue);
        return;
    }

    throw new Exception(format("Unrecognized statement type: %s", st));
}

// Convenience structure for generating if statements: represents one block
// (if, else if, or else)
class GeneratorIfBlock
{
    Node conditional;
    StatementBase block;
    string label;

    this(ConditionalBlock block, string label)
    {
        this.conditional = block.conditional;
        this.block = block.block;
        this.label = label;
    }

    this(StatementBase block, string label)
    {
        this.block = block;
        this.label = label;
    }
}

void generateConditional(GeneratorState state, Node conditional, string label)
{
    auto conditionalNode = generateNode(state, conditional);
    auto conditionalRegister = requireLocalInRegister(state, conditionalNode);
    auto register = conditionalRegister.expectSingleRegister();

    state.render(format("    test %s, %s", register, register));
    state.render(format("    je %s", label));

    state.freeTemp(conditionalRegister);
}

void generateWhile(GeneratorState state, While _while)
{
    state.render(format("%s:", _while.startLabel()));

    if (_while.conditional !is null)
    {
        auto conditional = renderNode(state,
                generateNode(state, _while.conditional));

        state.render(format("    test %s, %s", conditional, conditional));
        state.render(format("    je %s", _while.endLabel()));
    }

    // Loop body
    generateStatementBase(state, _while.block);

    state.render(format("    jmp %s", _while.startLabel()));
    state.render(format("%s:", _while.endLabel()));
}

void generateBreak(GeneratorState state, Break _break)
{
    auto _while = _break.containingWhile();

    if (_while is null)
    {
        throw new Exception("break found outside any while block");
    }

    state.render(format("    jmp %s", _while.endLabel()));
}

void generateContinue(GeneratorState state, Continue _continue)
{
    auto _while = _continue.containingWhile();

    if (_while is null)
    {
        throw new Exception("continue found outside any while block");
    }

    state.render(format("    jmp %s", _while.startLabel()));
}

void generateIf(GeneratorState state, If _if)
{
    // Put all the blocks in a standardized format for this process
    GeneratorIfBlock[] blocks;

    blocks ~= new GeneratorIfBlock(_if.ifBlock, state.addLabel("if_"));

    for (int i = 0; i < _if.elseIfBlocks.length; i++)
    {
        blocks ~= new GeneratorIfBlock(_if.elseIfBlocks[i],
                state.addLabel(format("else_if_%d_", i)));
    }

    if (_if.elseBlock !is null)
    {
        blocks ~= new GeneratorIfBlock(_if.elseBlock,
                                       state.addLabel("else_"));
    }

    // Start the real work
    auto endIfLabel = state.addLabel("end_if_");

    for (int i = 0; i < blocks.length; i++)
    {
        auto block = blocks[i];

        state.render(format("%s:", block.label));

        if (block.conditional !is null)
        {
            // The label to jump to if the comparison fails
            auto nextBlockLabel = endIfLabel;
            if (i + 1 < blocks.length)
            {
                nextBlockLabel = blocks[i + 1].label;
            }

            generateConditional(state, block.conditional, nextBlockLabel);
        }

        // Block: run if the conditional was true
        generateStatementBase(state, block.block);

        // Jump out of the if structure, unless we're already at the end
        if (i < blocks.length)
        {
            state.render(format("    jmp %s", endIfLabel));
        }
    }

    state.render(format("%s:", endIfLabel));
}

void generateBlock(GeneratorState state, Block block)
{
    foreach (statement; block.statements)
    {
        generateStatementBase(state, statement);
        state.temps = [];
    }

    cleanupBlock(state, block, null);
}

// When exiting a block (closing curly brace, continue, break) any arrays
// dynamically allocated before the given termination statement (or null if
// it's the end of the block) need to be freed.
void cleanupBlock(GeneratorState state, Block block, StatementBase st)
{
    LocalDeclaration firstDynamicArray = null;

    foreach (statement; block.statements)
    {
        if (statement == st)
        {
            break;
        }

        auto decl = cast(LocalDeclaration)statement;
        if (decl is null)
        {
            continue;
        }

        if (!decl.signature.type.isConcrete())
        {
            firstDynamicArray = decl;
            break;
        }
    }

    if (firstDynamicArray is null)
    {
        return;
    }

    auto local = state.findLocal(firstDynamicArray.signature.name);

    state.render(format("    mov rsp, %s", renderLocal(local)));
    state.render(format("    add rsp, 8"));
}

void generateDynamicArray(GeneratorState state, LocalDeclaration st)
{
    auto arraySizeNode = generateNode(state, st.signature.type.elements);
    auto arraySizeInRegister = requireLocalInRegister(state, arraySizeNode);
    auto arraySizeRegister = arraySizeInRegister.expectSingleRegister();

    auto arrayPtr = state.findLocal(st.signature.name);

    // Save value of the start of the newly-allocated data in the array's
    // pointer in the static area.
    state.render(format("    mov %s, rsp", renderLocal(arrayPtr)));
    state.render(format("    sub qword %s, 8", renderLocal(arrayPtr)));

    // Allocate the requested space
    state.render(format("    sub rsp, %s", arraySizeRegister));

    state.freeTemp(arraySizeInRegister);
}

void generateLocalDeclaration(GeneratorState state, LocalDeclaration st)
{
    if (!st.signature.type.isConcrete())
    {
        generateDynamicArray(state, st);
        return;
    }

    if (st.value is null)
    {
        return;
    }

    generateAssignmentShared(state,
            new Binding(st.signature, st.signature.name), st.value);
}

void generateStatement(GeneratorState state, Statement s)
{
    auto result = generateNode(state, s.expression);
}

void generateAssignment(GeneratorState state, Assignment a)
{
    generateAssignmentShared(state, a.lvalue, a.value);
}

void generateAssignmentShared(GeneratorState state, Node target,
                              Node expression)
{
    bool targetDereference = false;
    Local tempTarget = null;
    Local tempTarget2 = null;

    auto deref = cast(Dereference)target;
    if (deref !is null)
    {
        targetDereference = true;
        target = deref.source;
    }

    string targetRendered = null;
    Type targetType = null;

    auto indexer = cast(Indexer)target;
    IndexerResolveData indexerData;

    auto dot = cast(DotAccessor)target;

    auto sizeHint = typeToOpSize(expression.type);

    // Assigning to an indexer
    if (indexer !is null)
    {
        indexerData = resolveIndexer(state, indexer);
        targetRendered = indexerData.address;
        targetType = indexerData.sourceRegister.type.clone();
        targetType.pointer = false;
    }
    // Assigning to a dot accessor
    else if (dot !is null)
    {
        tempTarget = resolveDotAccessor(state, dot);
        targetRendered = format("[%s]", tempTarget.expectSingleRegister());
    }
    // Assigning to a binding
    else
    {
        auto targetBinding = cast(Binding)target;
        if (targetBinding is null)
        {
            throw new Exception(format("Can't assign to %s", target));
        }

        auto targetLocal = state.findLocal(targetBinding.name);
        if (targetLocal is null)
        {
            throw new Exception(format("Local %s not found",
                    targetBinding.name));
        }

        targetRendered = renderLocal(targetLocal);
        targetType = targetLocal.type;

        // Deal with dereference if necessary
        if (targetDereference)
        {
            tempTarget = state.addTemp(targetLocal.type);
            state.render(format("    mov %s, %s",
                    tempTarget.expectSingleRegister(), targetRendered));
            targetRendered = format("[%s]", tempTarget.expectSingleRegister());

            targetType = targetLocal.type.clone();
            targetType.pointer = false;
        }

        sizeHint = typeToOpSize(targetType);
    }

    auto value = generateNode(state, expression);
    auto valueRendered = renderNode(state, value);

    auto binding = cast(Binding)value;
    auto localValue = cast(Local)value;
    if (binding !is null)
    {
        tempTarget2 = state.addTemp(binding.type);
        auto tempTarget2Register = tempTarget2.expectSingleRegister();
        if (sizeHint != OpSize.Qword)
        {
            tempTarget2Register = convertRegisterSize(tempTarget2Register,
                    sizeHint);
        }
        state.render(format("    mov %s, %s", tempTarget2Register,
                            valueRendered));
        valueRendered = to!string(tempTarget2Register);
    }
    else if (localValue !is null && localValue.inRegister())
    {
        auto valueRegister = localValue.expectSingleRegister();
        if (sizeHint != OpSize.Qword)
        {
            valueRegister = convertRegisterSize(valueRegister, sizeHint);
        }
        valueRendered = to!string(valueRegister);
    }

    state.render(format("    mov %s %s, %s", sizeHint, targetRendered,
                        valueRendered));

    auto valueLocal = cast(Local)value;
    if (valueLocal !is null)
    {
        state.freeTemp(valueLocal);
    }

    if (tempTarget !is null)
    {
        state.freeTemp(tempTarget);
    }

    if (tempTarget2 !is null)
    {
        state.freeTemp(tempTarget2);
    }

    if (indexer !is null)
    {
        state.freeTemp(indexerData.sourceRegister);
        state.freeTemp(indexerData.indexerRegister);
    }
}

string renderImmediate(Literal literal)
{
    auto u64Literal = cast(U64Literal)literal;
    if (u64Literal !is null)
    {
        return to!string(u64Literal.value);
    }

    auto u8Literal = cast(U8Literal)literal;
    if (u8Literal !is null)
    {
        return to!string(u8Literal.value);
    }

    auto boolLiteral = cast(BoolLiteral)literal;
    if (boolLiteral !is null)
    {
        return boolLiteral.value ? "1" : "0";
    }

    throw new Exception(
            format("Can't render literal as immediate: %s", literal));
}

void generateReturn(GeneratorState state, Return r)
{
    auto value = generateNode(state, r.expression);

    // Move the return value into rax if it isn't already there
    auto literal = cast(Literal)value;
    if (literal !is null)
    {
        state.render(format("    mov rax, %s", renderImmediate(literal)));
    }
    else if (value !is null)
    {
        auto local = cast(Local)value;

        if (local is null)
        {
            auto binding = cast(Binding)value;
            if (binding !is null)
            {
                local = state.findLocal(binding.name);
            }
        }

        if (local is null)
        {
            throw new Exception(format("Can't return this: %s", value));
        }

        copyData(state, local.data, [Register.RAX, Register.RDX], false);
    }

    state.render("    mov rsp, rbp");
    state.render("    pop rbp");
    state.render("    ret");
}

// Generate a Node in an expression, doing any necessary setup to get it usable
// by the calling generator.
Node generateNode(GeneratorState state, Node node)
{
    // Special handling for operators
    auto operator = cast(Operator)node;
    if (operator !is null)
    {
        return generateOperator(state, operator);
    }

    // Special handling for function calls
    auto call = cast(Call)node;
    if (call !is null)
    {
        return generateCall(state, call);
    }

    // Special handling for calls
    auto typeCast = cast(Cast)node;
    if (typeCast !is null)
    {
        return generateCast(state, typeCast);
    }

    // Special handling for references
    auto reference = cast(Reference)node;
    if (reference !is null)
    {
        return generateReference(state, reference);
    }

    // Special handling for dereferences
    auto dereference = cast(Dereference)node;
    if (dereference !is null)
    {
        return generateDereference(state, dereference);
    }

    // Special handling for indexers
    auto indexer = cast(Indexer)node;
    if (indexer !is null)
    {
        return generateIndexer(state, indexer);
    }

    // Special handling for dot accessor
    auto accessor = cast(DotAccessor)node;
    if (accessor !is null)
    {
        return generateDotAccessor(state, accessor);
    }

    // Special handling for sizeof
    auto sizeof = cast(SizeOf)node;
    if (sizeof !is null)
    {
        return generateSizeOf(state, sizeof);
    }

    return node;
}

Node generateSizeOf(GeneratorState state, SizeOf sizeof)
{
    auto ret = state.addTemp(sizeof.type);

    state.render(format("    mov %s, %d", ret.expectSingleRegister(),
            sizeof.argument.baseTypeSize()));

    return ret;
}

bool isPrimitiveIntegral(Type type)
{
    return type.isPrimitive(Primitive.U64);
}

bool isLiteralInteger(GeneratorState state, Node node)
{
    auto literal = cast(Literal)node;
    return literal !is null && isPrimitiveIntegral(literal.type);
}

bool isBindingInteger(GeneratorState state, Node node)
{
    auto binding = cast(Binding)node;
    if (binding is null)
    {
        return false;
    }

    auto local = state.findLocal(binding.name);
    if (local is null)
    {
        return false;
    }

    if (local.type.pointer)
    {
        return false;
    }

    return isPrimitiveIntegral(local.type);
}

// Make a register copy of the given Binding/Local
Local requireCopyInRegister(GeneratorState state, Node node)
{
    auto ret = state.addTemp(node.type);
    auto register = ret.expectSingleRegister();
    register = convertRegisterSize(register, typeToOpSize(ret.type));
    ret.data = [new RegisterLocation(register)];

    // Special handling for arrays and non-pointer structs
    if (node.type.elements !is null ||
        (node.type.isStruct() && !node.type.pointer))
    {
        state.render(format("    lea %s, %s", register,
                            renderNode(state, node)));
    }
    else
    {
        auto opSize = typeToOpSize(ret.type);
        state.render(format("    mov %s %s, %s", opSize, register,
                            renderNode(state, node)));
    }

    return ret;
}

// If the given node is not in a register, move it into one
Local requireLocalInRegister(GeneratorState state, Node node)
{
    // If node is already a local, resolve it
    auto local = cast(Local)node;
    if (local is null)
    {
        auto binding = cast(Binding)node;
        if (binding !is null)
        {
            local = state.findLocal(binding.name);
            if (local is null)
            {
                throw new Exception(format("Can't find local %s",
                        binding.name));
            }
        }
    }

    // If it's already in a register, no work needed
    if (local !is null && local.inRegister())
    {
        return local;
    }

    // Not in a register: make a new temp and copy it there
    return requireCopyInRegister(state, node);
}

struct IndexerResolveData
{
    string address;

    // These temps can be freed by the caller after use of the address.
    Local sourceRegister;
    Local indexerRegister;
}

// Resolves an indexer to an addressing mode operand pointing to the area in
// memory where the indexed item is. This allows generators to read the value
// into a register when it's an rvalue and write to the memory address
// directly when it's an lvalue.
IndexerResolveData resolveIndexer(GeneratorState state, Indexer indexer)
{
    IndexerResolveData data;

    auto indexerNode = generateNode(state, indexer.index);
    data.indexerRegister = requireLocalInRegister(state, indexerNode);

    auto sourceNode = generateNode(state, indexer.source);
    data.sourceRegister = requireLocalInRegister(state, sourceNode);

    auto scale = data.sourceRegister.type.baseTypeSize();

    data.address = format("[%s + %d * %s]",
            data.sourceRegister.expectSingleRegister(), scale,
            data.indexerRegister.expectSingleRegister());

    return data;
}

Local generateIndexer(GeneratorState state, Indexer indexer)
{
    auto data = resolveIndexer(state, indexer);

    auto outputType = data.sourceRegister.type.clone();
    outputType.pointer = false;

    auto output = state.addTemp(outputType);

    state.render(format("    mov %s, %s", output.expectSingleRegister(),
            data.address));

    state.freeTemp(data.sourceRegister);
    state.freeTemp(data.indexerRegister);

    return output;
}

// Return a register with the address of the dot-accessed struct member
Local resolveDotAccessor(GeneratorState state, DotAccessor dot)
{
    Local containerRegister;

    auto containerDot = cast(DotAccessor)dot.container;
    if (containerDot is null)
    {
        auto containerNode = generateNode(state, dot.container);
        containerRegister = requireCopyInRegister(state, containerNode);
    }
    else
    {
        containerRegister = resolveDotAccessor(state, containerDot);
    }

    auto structType = cast(StructType)dot.container.type;
    if (structType is null)
    {
        throw new Exception(format(
                "Can't apply a dot accessor to a non-struct %s",
                dot.container.type));
    }

    // Calculate member offset
    int memberOffset = 0;
    foreach (member; structType.struct_.members)
    {
        if (member == dot.member)
        {
            break;
        }

        memberOffset += member.type.baseTypeSize();
    }

    state.render(format("    add %s, %d",
            containerRegister.expectSingleRegister(), memberOffset));

    return containerRegister;
}

Local generateDotAccessor(GeneratorState state, DotAccessor dot)
{
    auto member = resolveDotAccessor(state, dot);
    auto output = state.addTemp(dot.type);

    state.render(format("    mov %s, [%s]", output.expectSingleRegister(),
            member.expectSingleRegister()));

    state.freeTemp(member);

    return output;
}

Local generateDereference(GeneratorState state, Dereference dereference)
{
    auto sourceNode = generateNode(state, dereference.source);

    auto sourceBinding = cast(Binding)sourceNode;
    if (sourceBinding is null)
    {
        throw new Exception(format("Can't dereference node %s", sourceNode));
    }

    auto sourceLocal = state.findLocal(sourceBinding.name);
    if (sourceLocal is null)
    {
        throw new Exception(format("Can't find temp %s", sourceBinding.name));
    }

    auto outputType = sourceLocal.type.clone();
    outputType.pointer = false;

    auto outputLocal = state.addTemp(outputType);
    auto temp = state.addTemp(new PrimitiveType(Primitive.U64));

    state.render(format("    mov %s, %s", temp.expectSingleRegister(),
                        renderNode(state, sourceNode)));

    string targetRegister;
    if (outputType.isPrimitive(Primitive.U64))
    {
        targetRegister = to!string(outputLocal.expectSingleRegister());
    }
    else if (outputType.isPrimitive(Primitive.U8))
    {
        targetRegister = lowByte(outputLocal.expectSingleRegister());
    }
    else
    {
        throw new Exception(format("Can't dereference type %s", outputType));
    }

    state.render(format("    mov %s, [%s]", targetRegister,
            temp.expectSingleRegister()));
    state.freeTemp(temp);

    return outputLocal;
}

Local generateReference(GeneratorState state, Reference reference)
{
    auto sourceNode = generateNode(state, reference.source);

    auto sourceBinding = cast(Binding)sourceNode;
    if (sourceBinding is null)
    {
        throw new Exception(format("Can't reference node %s", sourceNode));
    }

    auto sourceLocal = state.findLocal(sourceBinding.name);
    if (sourceLocal is null)
    {
        throw new Exception(format("Can't find temp %s", sourceBinding.name));
    }

    if (sourceLocal.inRegister())
    {
        throw new Exception(format("Can't reference a register %s",
                                   sourceBinding.name));
    }

    auto outputType = sourceLocal.type.clone();
    outputType.pointer = true;

    auto outputLocal = state.addTemp(outputType);

    state.render(format("    lea %s, %s", outputLocal.expectSingleRegister(),
                        renderNode(state, sourceNode)));

    return outputLocal;
}

Local generateCast(GeneratorState state, Cast typeCast)
{
    auto unableToCast = new Exception(format("Unable to cast %s to %s",
            typeCast.target, typeCast.type));

    if (typeCast.type.isPrimitive(Primitive.U64) &&
        typeCast.target.type.isPrimitive(Primitive.U8))
    {
        return generateCastU8ToU64(state, typeCast);
    }

    if (typeCast.type.pointer ||
        typeCast.type.isPrimitive(Primitive.Bool))
    {
        if (isLiteralInteger(state, typeCast.target))
        {
            return generateCastLiteralIntegerToBool(state, typeCast);
        }

        if (isBindingInteger(state, typeCast.target))
        {
            return generateCastLocalIntegerToBool(state, typeCast);
        }
    }

    throw unableToCast;
}

Local generateCastU8ToU64(GeneratorState state, Cast typeCast)
{
    auto source = renderNode(state, generateNode(state, typeCast.target));
    auto target = state.addTemp(typeCast.type.clone());
    auto targetRegister = target.expectSingleRegister();

    state.render(format("    xor %s, %s", targetRegister, targetRegister));
    state.render(format("    mov %s, %s", lowByteRegister(targetRegister),
            source));

    return target;
}

Local generateCastLiteralIntegerToBool(GeneratorState state, Cast typeCast)
{
    bool foundMatch = false;
    bool castedValue;

    auto u64Literal = cast(U64Literal)typeCast.target;
    if (u64Literal !is null)
    {
        castedValue = u64Literal.value != 0;
        foundMatch = true;
    }

    if (!foundMatch)
    {
        throw new Exception(format("Can't convert %s to bool",
                            typeCast.target));
    }

    auto target = state.addTemp(new PrimitiveType(Primitive.Bool));
    auto targetRegister = target.expectSingleRegister();

    if (castedValue)
    {
        state.render(format("    mov %s, 1", targetRegister));
    }
    else
    {
        state.render(format("    xor %s, %s", targetRegister,
                            targetRegister));
    }

    return target;
}

Local generateCastLocalIntegerToBool(GeneratorState state, Cast typeCast)
{
    auto source = renderNode(state, generateNode(state, typeCast.target));
    auto target = state.addTemp(new PrimitiveType(Primitive.Bool));
    auto targetRegister = target.expectSingleRegister();

    state.render(format("    xor %s, %s", targetRegister, targetRegister));
    state.render(format("    cmp %s, 0", source));
    state.render(format("    setne %s", lowByte(targetRegister)));

    return target;
}

Local generateOperator(GeneratorState state, Operator operator)
{
    switch(operatorTypeToClass(operator.operatorType))
    {
        case OperatorClass.Math:
            return generateMathOperator(state, operator);
        case OperatorClass.Relational:
            return generateRelationalOperator(state, operator);
        case OperatorClass.Logical:
            return generateLogicalAndOperator(state, operator);
        default:
            throw new Exception(format("Unrecognized operator type: %s",
                                       operator.operatorType));
    }
}

Local generateMathOperator(GeneratorState state, Operator operator)
{
    auto leftNode = generateNode(state, operator.left);
    auto rightNode = generateNode(state, operator.right);

    auto left = renderNode(state, leftNode);
    auto right = renderNode(state, rightNode);

    auto raxTaken = state.registerTaken(Register.RAX);
    auto rdxTaken = state.registerTaken(Register.RDX);

    auto temp = state.addTemp(operator.type);
    auto tempRegister = temp.expectSingleRegister();

    if (operator.operatorType == OperatorType.Divide ||
        operator.operatorType == OperatorType.Modulo)
    {
        if (raxTaken)
        {
            state.render(format("    push rax"));
        }

        if (rdxTaken)
        {
            state.render(format("    push rdx"));
        }

        state.reserve(Register.RAX);
        state.reserve(Register.RDX);

        auto rightNodeRegister = requireLocalInRegister(state, rightNode);

        state.render(format("    mov rax, %s", left));
        state.render(format("    xor rdx, rdx"));
        state.render(format("    idiv %s",
                            rightNodeRegister.expectSingleRegister()));

        state.unreserve(Register.RDX);
        state.unreserve(Register.RAX);

        if (operator.operatorType == OperatorType.Divide)
        {
            state.render(format("    mov %s, rax", tempRegister));
        }

        if (operator.operatorType == OperatorType.Modulo)
        {
            state.render(format("    mov %s, rdx", tempRegister));
        }

        state.freeTemp(rightNodeRegister);

        if (rdxTaken)
        {
            state.render(format("    pop rdx"));
        }

        if (raxTaken)
        {
            state.render(format("    pop rax"));
        }

        return temp;
    }

    if (operator.operatorType == OperatorType.Multiply)
    {
        auto leftNodeRegister = requireLocalInRegister(state, leftNode);
        auto rightNodeRegister = requireLocalInRegister(state, rightNode);

        state.render(format("    mov %s, %s", tempRegister,
                leftNodeRegister.expectSingleRegister()));
        state.render(format("    imul %s, %s", tempRegister,
                rightNodeRegister.expectSingleRegister()));

        state.freeTemp(rightNodeRegister);
        state.freeTemp(leftNodeRegister);

        return temp;
    }

    state.render(format("    mov %s, %s", tempRegister, left));

    switch (operator.operatorType)
    {
        case OperatorType.Plus:
            state.render(format("    add %s, %s", tempRegister, right));
            break;
        case OperatorType.Minus:
            state.render(format("    sub %s, %s", tempRegister, right));
            break;
        case OperatorType.LeftShift:
            state.render(format("    shl %s, %s", tempRegister, right));
            break;
        case OperatorType.RightShift:
            state.render(format("    shr %s, %s", tempRegister, right));
            break;
        case OperatorType.BitwiseAnd:
            state.render(format("    and %s, %s", tempRegister, right));
            break;
        default:
            throw new Exception(format("Unrecognized math operator type: %s",
                                       operator.operatorType));
    }

    return temp;
}

string renderSizeHint(Node left, Node right)
{
    // If there's a literal involved make sure it's on the right side
    auto literal = cast(Literal)left;
    if (literal !is null)
    {
        auto temp = left;
        left = right;
        right = temp;
    }

    auto rightLiteral = cast(Literal)right;
    if (rightLiteral !is null)
    {
        return to!string(typeToOpSize(left.type)) ~ " ";
    }

    return "";
}

bool isInMemory(GeneratorState state, Node node)
{
    auto local = cast(Local)node;
    if (local is null)
    {
        auto binding = cast(Binding)node;
        if (binding !is null)
        {
            local = state.findLocal(binding.name);
        }
    }

    if (local is null)
    {
        return false;
    }

    return !local.inRegister();
}

Local generateRelationalOperator(GeneratorState state, Operator operator)
{
    auto leftNode = generateNode(state, operator.left);
    auto rightNode = generateNode(state, operator.right);

    if (isInMemory(state, leftNode) && isInMemory(state, rightNode))
    {
        leftNode = requireLocalInRegister(state, leftNode);
    }

    auto left = renderNode(state, leftNode);
    auto right = renderNode(state, rightNode);

    auto temp = state.addTemp(new PrimitiveType(Primitive.Bool));
    auto tempRegister = temp.expectSingleRegister();
    state.render(format("    xor %s, %s", tempRegister, tempRegister));

    auto sizeHint = renderSizeHint(leftNode, rightNode);
    state.render(format("    cmp %s%s, %s", sizeHint, left, right));

    string op = null;
    switch (operator.operatorType)
    {
        case OperatorType.Equality:
            op = "sete";
            break;
        case OperatorType.Inequality:
            op = "setne";
            break;
        case OperatorType.GreaterThan:
            op = "setg";
            break;
        case OperatorType.GreaterThanOrEqual:
            op = "setge";
            break;
        case OperatorType.LessThan:
            op = "setl";
            break;
        case OperatorType.LessThanOrEqual:
            op = "setle";
            break;
        default:
            throw new Exception(format(
                    "Unrecognized relational operator type: %s",
                    operator.type));
    }

    state.render(format("    %s %s", op, lowByte(tempRegister)));

    return temp;
}

Local generateLogicalAndOperator(GeneratorState state, Operator operator)
{
    auto temp = state.addTemp(new PrimitiveType(Primitive.Bool));
    auto tempRegister = temp.expectSingleRegister();
    state.render(format("    xor %s, %s", tempRegister, tempRegister));

    auto endComparison = state.addLabel("end_comparison_");

    // Left operand
    generateConditional(state, operator.left, endComparison);

    // Right operand
    generateConditional(state, operator.right, endComparison);

    // Both were true
    state.render(format("    mov %s, 1", tempRegister));

    // Either were false
    state.render(format("%s:", endComparison));

    return temp;
}

class NonRegisterArg
{
    Node node;
    Register target;

    this(Node node, Register target)
    {
        this.node = node;
        this.target = target;
    }
}

class DataMove
{
    DataLocation source;
    DataLocation target;

    this(DataLocation source, DataLocation target)
    {
        this.source = source;
        this.target = target;
    }
}

// Map source data locations to a list of target registers with optional
// spill-over into the stack
DataMove[] mapData(GeneratorState state, DataLocation[] sources,
        Register[] targetRegisters, bool spill)
{
    DataMove[] ret;
    int stackOffset;

    void allocate(DataLocation source, int bytes)
    {
        // If registers are left
        if (targetRegisters.length > 0)
        {
            auto r = targetRegisters[0];
            targetRegisters = targetRegisters[1 .. targetRegisters.length];

            // Skip move from one register to itself
            auto sourceRegister = cast(RegisterLocation)source;
            if (sourceRegister !is null && sourceRegister.register == r)
            {
                return;
            }

            ret ~= new DataMove(source, new RegisterLocation(r));
            return;
        }

        // Spill over to stack
        if (!spill)
        {
            throw new Exception("Data mapping spilled to stack unexpectedly");
        }

        ret ~= new DataMove(source, new StackLocation(stackOffset, bytes));
        stackOffset += bytes;
    }

    foreach (source; sources)
    {
        auto literal = cast(LiteralLocation)source;
        auto register = cast(RegisterLocation)source;
        auto stack = cast(StackLocation)source;
        auto dataSection = cast(DataSectionLocation)source;

        if (literal !is null || dataSection !is null)
        {
            allocate(source, 8);
        }
        else if (register !is null)
        {
            allocate(register, register.bytes);
        }
        else if (stack !is null)
        {
            int sourceOffset = stack.offset;
            int sourceLeft = stack.bytes;

            while (sourceLeft > 0)
            {
                // Stack portion -> register
                if (targetRegisters.length > 0)
                {
                    allocate(new StackLocation(sourceOffset, 8), 8);
                    sourceOffset += 8;
                    sourceLeft -= 8;

                    continue;
                }

                allocate(new StackLocation(sourceOffset, sourceLeft),
                        sourceLeft);

                break;
            }
        }
        else
        {
            throw new Exception(format("Unrecognized data location: %s",
                    source));
        }
    }

    return ret;
}

string renderOffset(int offset)
{
    if (offset == 0)
    {
        return "";
    }
    else if (offset > 0)
    {
        return format("+ %d", offset);
    }
    else
    {
        return format("- %d", offset * -1);
    }
}

DataLocation[] extractDataLocations(Node[] nodes)
{
    DataLocation[] ret;

    foreach (node; nodes)
    {
        auto literal = cast(Literal)node;
        if (literal !is null)
        {
            ret ~= new LiteralLocation(literal);
            continue;
        }

        auto local = cast(Local)node;
        if (local is null)
        {
            throw new Exception(format("Unrecognized node %s", node));
        }

        ret ~= local.data;
    }

    return ret;
}

DataMove[] extractMoves(ref DataMove[] moves, bool function(DataMove) p)
{
    DataMove[] ret;

    int i = 0;
    while (i < moves.length)
    {
        if (!p(moves[i]))
        {
            i++;
            continue;
        }

        ret ~= moves[i];
        moves = moves.remove(i);
    }

    return ret;
}

// Copy a list of data locations to registers, with optional stack spill
void copyData(GeneratorState state, DataLocation[] sources,
        Register[] targetRegisters, bool spill)
{
    // Map parameters to their targets for the call
    auto moves = mapData(state, sources, targetRegisters, true);

    // Extract moves destined for the stack
    auto toStack = extractMoves(moves, function(DataMove m) =>
            typeid(m.target) == typeid(StackLocation));

    // Perform any moves destined for stack
    foreach (move; toStack)
    {
        auto targetStack = cast(StackLocation)move.target;

        auto sourceRegister = cast(RegisterLocation)move.source;
        auto sourceStack = cast(StackLocation)move.source;
        auto sourceLiteral = cast(LiteralLocation)move.source;

        if (sourceRegister !is null)
        {
            state.render(format("    push %s", sourceRegister.register));
        }
        else if (sourceStack !is null)
        {
            auto tempRegister = state.reserveNextFreeRegister();

            auto offset = sourceStack.bytes - 8;
            while (offset >= 0)
            {
                state.render(format("    mov %s, [rbp %s]", tempRegister,
                        renderOffset(sourceStack.offset + offset)));
                state.render(format("    push %s", tempRegister));

                offset -= 8;
            }

            state.unreserve(tempRegister);
        }
        else if (sourceLiteral !is null)
        {
            state.render(format("    push %s",
                    renderNode(state, sourceLiteral.literal)));
        }
        else
        {
            throw new Exception(format("Unrecognized source %s", move.source));
        }
    }

    // Extract moves from one register to another
    auto registerToRegister = extractMoves(moves, function(DataMove m) =>
            typeid(m.source) == typeid(RegisterLocation) &&
            typeid(m.target) == typeid(RegisterLocation));

    // Build list of register moves
    RegisterMove[] registerMoves;
    foreach (move; registerToRegister)
    {
        auto sourceRegister = cast(RegisterLocation)move.source;
        auto targetRegister = cast(RegisterLocation)move.target;

        registerMoves ~= new RegisterMove(sourceRegister.register,
                targetRegister.register);
    }

    // Perform register-to-register moves
    shuffleRegisters(state, registerMoves);

    // Extract moves to registers from other sources
    auto toRegister = extractMoves(moves, function(DataMove m) =>
            typeid(m.target) == typeid(RegisterLocation));

    // Perform moves to registers from other sources
    foreach (move; toRegister)
    {
        auto targetRegister = cast(RegisterLocation)move.target;

        auto sourceStack = cast(StackLocation)move.source;
        auto sourceLiteral = cast(LiteralLocation)move.source;
        auto sourceDataSection = cast(DataSectionLocation)move.source;

        string sourceRendered;

        if (sourceStack !is null)
        {
            sourceRendered = format("[rbp %s]",
                    renderOffset(sourceStack.offset));
        }
        else if (sourceLiteral !is null)
        {
            sourceRendered = renderNode(state, sourceLiteral.literal);
        }
        else if (sourceDataSection !is null)
        {
            sourceRendered = format("[%s]", sourceDataSection.name);
        }
        else
        {
            continue;
        }

        state.render(format("    mov %s, %s", targetRegister.register,
                sourceRendered));
    }

    if (moves.length > 0)
    {
        throw new Exception("Unprocessed data moves left");
    }
}

Register[] prepareCallParams(GeneratorState state, Node[] params,
        Register[] registerList)
{
    // Generate all parameters
    for (int i = 0; i < params.length; i++)
    {
        auto generated = generateNode(state, params[i]);

        auto binding = cast(Binding)generated;

        if (binding !is null)
        {
            auto bindingLocal = state.findLocal(binding.name);

            if (bindingLocal is null)
            {
                throw new Exception(format("Can't find local %s",
                        binding.name));
            }

            params[i] = bindingLocal;
            continue;
        }

        auto literal = cast(Literal)generated;
        auto local = cast(Local)generated;

        if (local is null && literal is null)
        {
            throw new Exception(format(
                    "Parameter didn't generate into a local or literal: %s",
                    params[i]));
        }

        params[i] = generated;
    }

    // Save caller-preserved registers
    auto callerPreserved = state.callerPreservedRegistersInUse();
    foreach (p; callerPreserved)
    {
        state.generatePush(p);
    }

    // Place params where they need to be for the call
    copyData(state, extractDataLocations(params), registerList, true);

    return callerPreserved;
}

Local cleanupCall(GeneratorState state, Type returnType,
                  Register[] callerPreserved)
{
    // Save return value (in rax) to a new temp variable
    auto temp = state.addTemp(returnType);
    auto tempRegister = temp.expectSingleRegister();
    if (tempRegister != Register.RAX)
    {
        state.render(format("    mov %s, rax", tempRegister));
    }

    // Restore caller-preserved registers
    for (int i = cast(int)callerPreserved.length - 1; i >= 0; i--)
    {
        state.generatePop(callerPreserved[i]);
    }

    return temp;
}

Local generateCall(GeneratorState state, Call call)
{
    if (call.functionName == "syscall")
    {
        return generateSysCall(state, call);
    }

    if (call.functionName == "variadic")
    {
        return generateVariadic(state, call);
    }

    auto callerPreserved = prepareCallParams(state, call.parameters,
            callRegisters);

    // Make the actual call
    auto func = call.targetSignature;
    state.render(format("    call %s", renderName(func)));

    // Make sure the function gets listed as an extern
    auto bshiftFunc = cast(Function)func;
    auto funcLocal = bshiftFunc !is null &&
                     bshiftFunc.signature.mod == state.mod;
    auto bshiftMethod = cast(Method)func;
    auto methodLocal = bshiftMethod !is null &&
                       bshiftMethod.signature.mod == state.mod;
    if (!funcLocal && !methodLocal)
    {
        state.addExtern(renderName(func));
    }

    return cleanupCall(state, func.returnType, callerPreserved);
}

Local generateSysCall(GeneratorState state, Call call)
{
    auto callerPreserved = prepareCallParams(state, call.parameters[1..$],
            syscallRegisters);

    // Handle the system call code, which needs to go in rax
    auto callCode = generateNode(state, call.parameters[0]);
    state.render(format("    mov rax, %s", renderNode(state, callCode)));

    // Make the system call
    state.render(format("    syscall"));

    return cleanupCall(state, new PrimitiveType(Primitive.U64),
            callerPreserved);
}

Local generateVariadic(GeneratorState state, Call call)
{
    auto func = call.containingFunction();

    if (func is null)
    {
        throw new Exception("variadic() called outside of a function");
    }

    if (!func.variadic)
    {
        throw new Exception("variadic() called from a non-variadic function");
    }

    auto indexNode = generateNode(state, call.parameters[0]);
    auto index = requireLocalInRegister(state, indexNode);
    auto indexRegister = index.expectSingleRegister();

    auto ret = state.addTemp(new PrimitiveType(Primitive.U64));

    auto inStack = state.addLabel("variadic_stack_");
    auto end = state.addLabel("variadic_end_");

    // Skip non-variadic parameters
    state.render(format("    add %s, %d", indexRegister,
                                          func.parameters.length));

    // Different logic for stack- vs register-passed arguments
    state.render(format("    cmp %s, 5", indexRegister));
    state.render(format("    jg %s", inStack));

    // Parameter was passed through a register
    state.render(format("    neg %s", indexRegister));
    state.render(format("    jmp %s", end));

    // Parameter was passed on the stack
    state.render(format("%s:", inStack));
    state.render(format("    sub %s, 3", indexRegister));

    // End if
    state.render(format("%s:", end));
    state.render(format("    mov %s, [rbp + 8 * %s - 8]",
                        ret.expectSingleRegister(), indexRegister));

    state.freeTemp(index);

    return ret;
}

string renderLocal(Local local)
{
    if (local.data.length != 1)
    {
        throw new Exception("Can't render local with more than 1 register");
    }

    auto registerLocation = cast(RegisterLocation)local.data[0];
    if (registerLocation !is null)
    {
        if (!local.type.pointer && local.type.baseTypeSize() == 1)
        {
            return format("%s", lowByte(registerLocation.register));
        }

        return format("%s", registerLocation.register);
    }

    auto stackLocation = cast(StackLocation)local.data[0];
    if (stackLocation !is null)
    {
        return renderStackLocation(stackLocation.offset);
    }

    auto dataLocation = cast(DataSectionLocation)local.data[0];
    if (dataLocation !is null)
    {
        return format("[%s]", local.name);
    }

    throw new Exception(format("Unknown location %s", local.data[0]));
}

string renderNode(GeneratorState state, Node node)
{
    auto loc = cast(Local)node;
    if (loc !is null)
    {
        return renderLocal(loc);
    }

    auto binding = cast(Binding)node;
    if (binding !is null)
    {
        auto local = state.findLocal(binding.name);

        if (local is null)
        {
            throw new Exception(format("Can't find local %s", binding.name));
        }

        return renderLocal(local);
    }

    auto u64Literal = cast(U64Literal)node;
    if (u64Literal !is null)
    {
        return format("%d", u64Literal.value);
    }

    auto u8Literal = cast(U8Literal)node;
    if (u8Literal !is null)
    {
        return format("%d", u8Literal.value);
    }

    auto boolLiteral = cast(BoolLiteral)node;
    if (boolLiteral !is null)
    {
        return boolLiteral.value ? "1" : "0";
    }

    auto stringLiteral = cast(StringLiteral)node;
    if (stringLiteral !is null)
    {
        auto literalName = state.addStringLiteral(stringLiteral.value);

        // TODO: free temp
        auto temp = state.addTemp(new PrimitiveType(Primitive.U64));
        auto tempRegister = temp.expectSingleRegister();
        state.render(format("    mov %s, %s", tempRegister, literalName));

        return to!string(tempRegister);
    }

    auto operator = cast(Operator)node;
    if (operator !is null)
    {
        auto local = generateOperator(state, operator);
        return format("%s", local.expectSingleRegister());
    }

    throw new Exception(format("Node %s unrecognized", node));
}

string generateBinding(GeneratorState state, Binding binding)
{
    auto local = state.findLocal(binding.name);

    if (local is null)
    {
        throw new Exception(format("Local %s not found", binding.name));
    }

    if (!local.inRegister())
    {
        throw new Exception(
                format("Local %s not in a register", binding.name));
    }

    return format("%s", local.expectSingleRegister());
}

// Register shuffling ---------------------------------------------------------

class RegisterMove
{
    Register source;
    Register target;

    this(Register source, Register target)
    {
        this.source = source;
        this.target = target;
    }

    override string toString()
    {
        return format("%s -> %s", this.source, this.target);
    }
}

RegisterMove findRegisterMove(RegisterMove[] moves, Register source)
{
    foreach (move; moves)
    {
        if (move.source == source)
        {
            return move;
        }
    }

    return null;
}

bool isHandled(Register[] handled, Register register)
{
    foreach (check; handled)
    {
        if (check == register)
        {
            return true;
        }
    }

    return false;
}

void shuffleRegisters(GeneratorState state, RegisterMove[] moves)
{
    Register[] handled;

    foreach (start; moves)
    {
        if (isHandled(handled, start.source))
        {
            continue;
        }

        Register[] chain;
        chain ~= start.source;
        handled ~= start.source;

        auto current = start;
        bool circular = false;

        while (true)
        {
            chain ~= current.target;
            handled ~= current.target;

            auto next = findRegisterMove(moves, current.target);

            // End of chain
            if (next is null)
            {
                break;
            }

            current = next;

            // Detect circular chain
            if (current.target == start.source)
            {
                circular = true;
                break;
            }
        }

        // Simple (non-circular) chain
        if (!circular)
        {
            for (auto i = chain.length - 1; i > 0; i--)
            {
                state.render(format("    mov %s, %s", chain[i], chain[i - 1]));
            }

            continue;
        }

        // Circular chain with only two nodes (optimize to use xchg)
        if (chain.length == 2)
        {
            state.render(format("    xchg %s, %s", chain[0], chain[1]));
            continue;
        }

        // Circular chain
        auto tempLocal = state.addTemp(new PrimitiveType(Primitive.U64));
        auto temp = tempLocal.expectSingleRegister();

        state.render(format("    mov %s, %s", temp, chain[$-1]));

        for (auto i = chain.length - 1; i > 0; i--)
        {
            state.render(format("    mov %s, %s", chain[i], chain[i - 1]));
        }

        state.render(format("    mov %s, %s", chain[0], temp));

        state.freeTemp(tempLocal);
    }
}
