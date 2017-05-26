import std.stdio;
import std.format;
import std.conv;
import std.array;

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

enum Location
{
    Register,
    Stack,
    DataSection,
}

class Local : Node
{
    string name;
    Location location;
    Register register; // For Location.Register
    int stackOffset;   // For Location.Stack

    this(Type type, string name)
    {
        this.type = type;
        this.name = name;
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

        for (int i = 0; i < this.locals.length; i++)
        {
            if (this.locals[i].location == Location.Register &&
                isCallerPreserved(this.locals[i].register))
            {
                ret ~= this.locals[i].register;
            }
        }

        for (int i = 0; i < this.temps.length; i++)
        {
            if (this.temps[i].location == Location.Register &&
                isCallerPreserved(this.temps[i].register))
            {
                ret ~= this.temps[i].register;
            }
        }

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
        foreach (local; this.locals)
        {
            if (local.location == Location.Register &&
                local.register == register)
            {
                return true;
            }
        }

        foreach (temp; this.temps)
        {
            if (temp.location == Location.Register &&
                temp.register == register)
            {
                return true;
            }
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

    Local addLocal(Type type, string name)
    {
        auto local = new Local(type, name);

        local.location = Location.Register;
        local.register = this.findFreeRegister();

        this.locals ~= local;

        return local;
    }

    Local addTemp(Type type)
    {
        auto ret = new Local(type, format("temp%d", this.nextTempIndex++));

        ret.location = Location.Register;
        ret.register = this.findFreeRegister();

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

                ret.location = Location.DataSection;

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

// Place a parameter in the appropriate location (register etc) based on its
// index within the parameter list
void placeParameter(Local local, Register[] registerList, int index)
{
    if (index < 0)
    {
        throw new Exception(format("Invalid parameter index %d", index));
    }

    if (index < registerList.length)
    {
        local.location = Location.Register;
        local.register = registerList[index];

        return;
    }

    local.location = Location.Stack;
    local.stackOffset = (index - cast(int)registerList.length) * 8 + 16;
}

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

        argc.location = Location.Stack;
        argc.stackOffset = 16;

        state.locals ~= argc;

        auto argv = new Local(func.signature.parameters[1].type,
                              func.signature.parameters[1].name);

        argv.location = Location.Stack;
        argv.stackOffset = 24;

        state.locals ~= argv;
    }

    // Function parameters need to be added to locals
    if (!mainWithArgs)
    {
        for (int i = 0; i < func.signature.parameters.length; i++)
        {
            auto local = new Local(func.signature.parameters[i].type,
                                   func.signature.parameters[i].name);

            placeParameter(local, callRegisters, i);

            if (i < callRegisters.length)
            {
                stackOffset -= typeSize(func.signature.parameters[i].type);
                local.stackOffset = stackOffset;
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

        local.location = Location.Stack;

        auto size = 0;
        if (local.type.isConcrete())
        {
            size = typeSize(local.type);
        }
        else
        {
            // Dynamic arrays need a single pointer in prologue space, which
            // will later pointer to their actual data region, allocated higher
            // in the stack.
            size = 8;
        }

        stackOffset -= size;
        local.stackOffset = stackOffset;

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

    // Copy locals into the stack
    foreach (local; state.locals)
    {
        if (local.location == Location.Register)
        {
            state.render(format("    mov %s, %s",
                    renderStackLocation(local.stackOffset), local.register));

            local.location = Location.Stack;
        }
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
    if (st.line !is null)
    {
        //state.render(format("; %s", st.line.source));
    }

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

    state.render(format("    test %s, %s", conditionalRegister.register,
                                           conditionalRegister.register));
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
    auto arraySizeRegister = requireLocalInRegister(state, arraySizeNode);

    auto arrayPtr = state.findLocal(st.signature.name);

    // Save value of the start of the newly-allocated data in the array's
    // pointer in the static area.
    state.render(format("    mov %s, rsp", renderLocal(arrayPtr)));
    state.render(format("    sub qword %s, 8", renderLocal(arrayPtr)));

    // Allocate the requested space
    state.render(format("    sub rsp, %s", arraySizeRegister.register));

    state.freeTemp(arraySizeRegister);
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
        targetRendered = format("[%s]", tempTarget.register);
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
            state.render(format("    mov %s, %s", tempTarget.register,
                                targetRendered));
            targetRendered = format("[%s]", tempTarget.register);

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
        auto tempTarget2Register = tempTarget2.register;
        if (sizeHint != OpSize.Qword)
        {
            tempTarget2Register = convertRegisterSize(tempTarget2.register,
                    sizeHint);
        }
        state.render(format("    mov %s, %s", tempTarget2Register,
                            valueRendered));
        valueRendered = to!string(tempTarget2Register);
    }
    else if (localValue !is null && localValue.location == Location.Register)
    {
        auto valueRegister = localValue.register;
        if (sizeHint != OpSize.Qword)
        {
            valueRegister = convertRegisterSize(valueRegister, sizeHint);
        }
        valueRendered = to!string(valueRegister);
    }

    state.render(format("    mov %s%s, %s", sizeHint, targetRendered,
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

        if (local.location != Location.Register ||
            local.register != Register.RAX)
        {
            auto localRendered = renderLocal(local);
            auto opSize = typeToOpSize(local.type);

            auto target = Register.RAX;

            if (opSize != OpSize.Qword)
            {
                target = convertRegisterSize(target, opSize);
                state.render(format("    xor rax, rax"));
            }

            state.render(format("    mov %s, %s", target, localRendered));
        }
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

    state.render(format("    mov %s, %d", ret.register,
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

// If the given node is not in a register, move it into one
Local requireLocalInRegister(GeneratorState state, Node node)
{
    // If node is already a local, resolve it
    auto binding = cast(Binding)node;
    if (binding !is null)
    {
        auto local = state.findLocal(binding.name);
        if (local is null)
        {
            throw new Exception(format("Can't find local %s", binding.name));
        }

        // If it's already in a register, no work needed
        if (local.location == Location.Register)
        {
            return local;
        }
    }

    // Not in a register: make a new temp and copy it there
    auto ret = state.addTemp(node.type);
    ret.register = convertRegisterSize(ret.register, typeToOpSize(ret.type));

    // Special handling for arrays and non-pointer structs
    if (node.type.elements !is null ||
        (node.type.isStruct() && !node.type.pointer))
    {
        state.render(format("    lea %s, %s", ret.register,
                            renderNode(state, node)));
    }
    else
    {
        auto opSize = typeToOpSize(ret.type);
        state.render(format("    mov %s %s, %s", opSize, ret.register,
                            renderNode(state, node)));
    }

    return ret;
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

    data.address = format("[%s + %d * %s]", data.sourceRegister.register,
                          scale, data.indexerRegister.register);

    return data;
}

Local generateIndexer(GeneratorState state, Indexer indexer)
{
    auto data = resolveIndexer(state, indexer);

    auto outputType = data.sourceRegister.type.clone();
    outputType.pointer = false;

    auto output = state.addTemp(outputType);

    state.render(format("    mov %s, %s", output.register, data.address));

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
        containerRegister = requireLocalInRegister(state, containerNode);
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

    state.render(format("    add %s, %d", containerRegister.register,
                        memberOffset));

    return containerRegister;
}

Local generateDotAccessor(GeneratorState state, DotAccessor dot)
{
    auto member = resolveDotAccessor(state, dot);
    auto output = state.addTemp(dot.type);

    state.render(format("    mov %s, [%s]", output.register, member.register));

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

    if (sourceLocal.location == Location.Register)
    {
        throw new Exception(format("Can't dereference a register %s",
                                   sourceBinding.name));
    }

    auto outputType = sourceLocal.type.clone();
    outputType.pointer = false;

    auto outputLocal = state.addTemp(outputType);
    auto temp = state.addTemp(new PrimitiveType(Primitive.U64));

    state.render(format("    mov %s, %s", temp.register,
                        renderNode(state, sourceNode)));

    string targetRegister;
    if (outputType.isPrimitive(Primitive.U64))
    {
        targetRegister = to!string(outputLocal.register);
    }
    else if (outputType.isPrimitive(Primitive.U8))
    {
        targetRegister = lowByte(outputLocal.register);
    }
    else
    {
        throw new Exception(format("Can't dereference type %s", outputType));
    }

    state.render(format("    mov %s, [%s]", targetRegister, temp.register));
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

    if (sourceLocal.location == Location.Register)
    {
        throw new Exception(format("Can't reference a register %s",
                                   sourceBinding.name));
    }

    auto outputType = sourceLocal.type.clone();
    outputType.pointer = true;

    auto outputLocal = state.addTemp(outputType);

    state.render(format("    lea %s, %s", outputLocal.register,
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

    state.render(format("    xor %s, %s", target.register, target.register));
    state.render(format("    mov %s, %s", lowByteRegister(target.register),
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

    if (castedValue)
    {
        state.render(format("    mov %s, 1", target.register));
    }
    else
    {
        state.render(format("    xor %s, %s", target.register,
                            target.register));
    }

    return target;
}

Local generateCastLocalIntegerToBool(GeneratorState state, Cast typeCast)
{
    auto source = renderNode(state, generateNode(state, typeCast.target));
    auto target = state.addTemp(new PrimitiveType(Primitive.Bool));

    state.render(format("    xor %s, %s", target.register, target.register));
    state.render(format("    cmp %s, 0", source));
    state.render(format("    setne %s", lowByte(target.register)));

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
        state.render(format("    idiv %s", rightNodeRegister.register));

        state.unreserve(Register.RDX);
        state.unreserve(Register.RAX);

        if (operator.operatorType == OperatorType.Divide)
        {
            state.render(format("    mov %s, rax", temp.register));
        }

        if (operator.operatorType == OperatorType.Modulo)
        {
            state.render(format("    mov %s, rdx", temp.register));
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

    state.render(format("    mov %s, %s", temp.register, left));

    switch (operator.operatorType)
    {
        case OperatorType.Plus:
            state.render(format("    add %s, %s", temp.register, right));
            break;
        case OperatorType.Minus:
            state.render(format("    sub %s, %s", temp.register, right));
            break;
        case OperatorType.Asterisk:
            state.render(format("    imul %s, %s", temp.register, right));
            break;
        case OperatorType.LeftShift:
            state.render(format("    shl %s, %s", temp.register, right));
            break;
        case OperatorType.RightShift:
            state.render(format("    shr %s, %s", temp.register, right));
            break;
        case OperatorType.BitwiseAnd:
            state.render(format("    and %s, %s", temp.register, right));
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

    return local.location != Location.Register;
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
    state.render(format("    xor %s, %s", temp.register, temp.register));

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

    state.render(format("    %s %s", op, lowByte(temp.register)));

    return temp;
}

Local generateLogicalAndOperator(GeneratorState state, Operator operator)
{
    auto temp = state.addTemp(new PrimitiveType(Primitive.Bool));
    state.render(format("    xor %s, %s", temp.register, temp.register));

    auto endComparison = state.addLabel("end_comparison_");

    // Left operand
    generateConditional(state, operator.left, endComparison);

    // Right operand
    generateConditional(state, operator.right, endComparison);

    // Both were true
    state.render(format("    mov %s, 1", temp.register));

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

Register[] prepareCallParams(GeneratorState state, Node[] params,
        Register[] registerList)
{
    // Generate all parameters
    for (int i = 0; i < params.length; i++)
    {
        params[i] = generateNode(state, params[i]);
    }

    // Save caller-preserved registers
    // TODO: optimize
    Register[] callerPreserved = state.callerPreservedRegistersInUse();
    for (auto i = 0; i < callerPreserved.length; i++)
    {
        state.generatePush(callerPreserved[i]);
    }

    // Push stack arguments in reverse order
    if (params.length > 0)
    {
        for (int i = cast(int)params.length - 1; i >= registerList.length; i--)
        {
            state.render(format("    push %s", renderNode(state, params[i])));
        }
    }

    // Build list of parameters in registers and where they have to move for
    // the call to work.
    RegisterMove[] registerMoves;
    NonRegisterArg[] nonRegisterArgs;
    for (int i = 0; i < params.length && i < registerList.length; i++)
    {
        auto targetRegister = registerList[i];

        auto binding = cast(Binding)params[i];
        if (binding is null)
        {
            nonRegisterArgs ~= new NonRegisterArg(params[i], targetRegister);
            continue;
        }

        auto local = state.findLocal(binding.name);
        if (local is null)
        {
            throw new Exception(format("Local %s not found", binding.name));
        }

        if (local.location != Location.Register)
        {
            nonRegisterArgs ~= new NonRegisterArg(params[i], targetRegister);
            continue;
        }

        registerMoves ~= new RegisterMove(local.register, targetRegister);
    }

    shuffleRegisters(state, registerMoves);

    // Pass args which aren't currently located in registers
    foreach (arg; nonRegisterArgs)
    {
        state.render(format("    mov %s, %s",
                            arg.target, renderNode(state, arg.node)));
    }

    return callerPreserved;
}

Local cleanupCall(GeneratorState state, Type returnType,
                  Register[] callerPreserved)
{
    // Save return value (in rax) to a new temp variable
    auto temp = state.addTemp(returnType);
    if (temp.register != Register.RAX)
    {
        state.render(format("    mov %s, rax", temp.register));
    }

    // Restore caller-preserved registers
    // TODO: optimize
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

    auto ret = state.addTemp(new PrimitiveType(Primitive.U64));

    auto inStack = state.addLabel("variadic_stack_");
    auto end = state.addLabel("variadic_end_");

    // Skip non-variadic parameters
    state.render(format("    add %s, %d", index.register,
                                          func.parameters.length));

    // Different logic for stack- vs register-passed arguments
    state.render(format("    cmp %s, 5", index.register));
    state.render(format("    jg %s", inStack));

    // Parameter was passed through a register
    state.render(format("    neg %s", index.register));
    state.render(format("    jmp %s", end));

    // Parameter was passed on the stack
    state.render(format("%s:", inStack));
    state.render(format("    sub %s, 3", index.register));

    // End if
    state.render(format("%s:", end));
    state.render(format("    mov %s, [rbp + 8 * %s - 8]", ret.register,
                        index.register));

    state.freeTemp(index);

    return ret;
}

string renderLocal(Local local)
{
    switch (local.location)
    {
        case Location.Register:
            if (!local.type.pointer && local.type.baseTypeSize() == 1)
            {
                return format("%s", lowByte(local.register));
            }

            return format("%s", local.register);
        case Location.Stack:
            return renderStackLocation(local.stackOffset);
        case Location.DataSection:
            return format("[%s]", local.name);
        default:
            throw new Exception(format("Unknown location %s", local.location));
    }
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
        state.render(format("    mov %s, %s", temp.register, literalName));

        return to!string(temp.register);
    }

    auto operator = cast(Operator)node;
    if (operator !is null)
    {
        auto local = generateOperator(state, operator);
        return format("%s", local.register);
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

    if (local.location != Location.Register)
    {
        throw new Exception(
                format("Local %s not in a register", binding.name));
    }

    return format("%s", local.register);
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
        auto temp = tempLocal.register;

        state.render(format("    mov %s, %s", temp, chain[$-1]));

        for (auto i = chain.length - 1; i > 0; i--)
        {
            state.render(format("    mov %s, %s", chain[i], chain[i - 1]));
        }

        state.render(format("    mov %s, %s", chain[0], temp));

        state.freeTemp(tempLocal);
    }
}
