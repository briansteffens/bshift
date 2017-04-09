import std.stdio;
import std.format;
import std.conv;

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
}

string lowByte(Register full)
{
    switch (full)
    {
        case Register.RAX:
            return "al";
        case Register.RBX:
            return "bl";
        case Register.RCX:
            return "cl";
        case Register.RDX:
            return "dl";
        case Register.RSI:
            return "sil";
        case Register.RDI:
            return "dil";
        case Register.R8:
            return "r8b";
        case Register.R9:
            return "r9b";
        case Register.R10:
            return "r10b";
        case Register.R11:
            return "r11b";
        case Register.R12:
            return "r12b";
        case Register.R13:
            return "r13b";
        case Register.R14:
            return "r14b";
        case Register.R15:
            return "r15b";
        default:
            throw new Exception(
                    format("Can't find the low byte for %s", full));
    }
}

enum OpSize
{
    Byte,
    Word,
    Dword,
    Qword,
}

OpSize primitiveToOpSize(PrimitiveType t)
{
    switch (t)
    {
        case PrimitiveType.Bool:
            return OpSize.Byte;
        case PrimitiveType.ULong:
            return OpSize.Qword;
        default:
            throw new Exception(format("Unknown type %s", t));
    }
}

OpSize typeToOpSize(Type t)
{
    if (t.pointer)
    {
        return OpSize.Qword;
    }

    return primitiveToOpSize(t.primitive);
}

enum Location
{
    Register,
    Stack,
}

class Local
{
    string name;
    Type type;
    Location location;
    Register register;
    int stackOffset;

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

PrimitiveType getType(GeneratorState state, Node node)
{
    auto literal = cast(Literal)node;
    if (literal !is null)
    {
        return literal.type;
    }

    throw new Exception(format("Can't figure out the type for Node %s", node));
}

class GeneratorState
{
    Module mod;

    string[] output;

    Local[] locals;
    Local[] temps;

    string[] labels;

    int nextTempIndex = 0;

    this(Module mod)
    {
        this.mod = mod;
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
        this.output ~= format("    push %s", r);
    }

    void generatePop(Register r)
    {
        this.output ~= format("    pop %s", r);
    }

    bool registerTaken(Register register)
    {
        for (int i = 0; i < this.locals.length; i++)
        {
            if (this.locals[i].location == Location.Register &&
                this.locals[i].register == register)
            {
                return true;
            }
        }

        for (int i = 0; i < this.temps.length; i++)
        {
            if (this.temps[i].location == Location.Register &&
                this.temps[i].register == register)
            {
                return true;
            }
        }

        return false;
    }

    Register findFreeRegister()
    {
        Register ret = Register.min;

        while (registerTaken(ret))
        {
            if (ret == Register.max)
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
            if (existing == temp)
            {
                newTemps ~= existing;
            }
        }

        this.temps = newTemps;
    }

    Local findLocal(string name)
    {
        // Look in locals
        for (int i = 0; i < this.locals.length; i++)
        {
            if (this.locals[i].name == name)
            {
                return this.locals[i];
            }
        }

        // Look in temps
        for (int i = 0; i < this.temps.length; i++)
        {
            if (this.temps[i].name == name)
            {
                return this.temps[i];
            }
        }

        return null;
    }
}

string renderFunctionName(string name)
{
    return format("function_%s", name);
}

string[] generate(Module mod)
{
    auto state = new GeneratorState(mod);

    state.output ~= "section .text";

    for (int i = 0; i < mod.functions.length; i++)
    {
        generateFunction(state, mod.functions[i]);
    }

    // Bootstrap the main function
    state.output ~= "global _start";
    state.output ~= "_start:";
    state.output ~= format("    call %s", renderFunctionName("main"));
    state.output ~= "    mov rdi, rax";
    state.output ~= "    mov rax, 60";
    state.output ~= "    syscall";

    return state.output;
}

// Figure out which register a parameter should be in based on its index in
// the parameter list
Register parameterRegister(int index)
{
    switch (index)
    {
        case 0:
            return Register.RDI;
        case 1:
            return Register.RSI;
        case 2:
            return Register.RDX;
        case 3:
            return Register.RCX;
        case 4:
            return Register.R8;
        case 5:
            return Register.R9;
        default:
            throw new Exception(format(
                    "Not enough registers for parameter %d", index));
    }
}

// Place a parameter in the appropriate location (register etc) based on its
// index within the parameter list
void placeParameter(Local local, int index)
{
    local.location = Location.Register;
    local.register = parameterRegister(index);
}

void generateFunction(GeneratorState state, Function func)
{
    int stackOffset = 0;

    // Function parameters need to be added to locals
    for (int i = 0; i < func.parameters.length; i++)
    {
        auto local = new Local(func.parameters[i].type,
                               func.parameters[i].name);

        placeParameter(local, i);

        stackOffset += typeSize(func.parameters[i].type);
        local.stackOffset = stackOffset;

        state.locals ~= local;
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

        stackOffset += typeSize(decl.signature.type);
        local.stackOffset = stackOffset;

        state.locals ~= local;
    }

    // Function prologue
    state.output ~= format("%s:", renderFunctionName(func.name));
    state.output ~= format("    push rbp");
    state.output ~= format("    mov rbp, rsp");

    if (stackOffset > 0)
    {
        state.output ~= format("    sub rsp, %s", stackOffset);
    }

    // Copy locals into the stack
    foreach (local; state.locals)
    {
        if (local.location == Location.Register)
        {
            state.output ~= format("mov [rbp - %d], %s",
                    local.stackOffset, local.register);

            local.location = Location.Stack;
        }
    }

    for (int i = 0; i < func.block.statements.length; i++)
    {
        generateStatement(state, func.block.statements[i]);
    }

    state.locals.length = 0;
    state.temps.length = 0;
}

void generateStatement(GeneratorState state, Statement st)
{
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

    throw new Exception(format("Unrecognized statement type: %s", st));
}

// Convenience structure for generating if statements: represents one block
// (if, else if, or else)
class GeneratorIfBlock
{
    Node conditional;
    Statement block;
    string label;

    this(ConditionalBlock block, string label)
    {
        this.conditional = block.conditional;
        this.block = block.block;
        this.label = label;
    }

    this(Statement block, string label)
    {
        this.block = block;
        this.label = label;
    }
}

void generateWhile(GeneratorState state, While _while)
{
    auto startWhileLabel = state.addLabel("while_start_");
    auto endWhileLabel = state.addLabel("while_end_");

    state.output ~= format("%s:", startWhileLabel);

    if (_while.conditional !is null)
    {
        auto conditional = renderNode(state,
                generateNode(state, _while.conditional));

        state.output ~= format("    test %s, %s", conditional, conditional);
        state.output ~= format("    je %s", endWhileLabel);
    }

    // Loop body
    generateStatement(state, _while.block);

    state.output ~= format("    jmp %s", startWhileLabel);
    state.output ~= format("%s:", endWhileLabel);
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

        state.output ~= format("%s:", block.label);

        if (block.conditional !is null)
        {
            // The label to jump to if the comparison fails
            auto nextBlockLabel = endIfLabel;
            if (i + 1 < blocks.length)
            {
                nextBlockLabel = blocks[i + 1].label;
            }

            auto conditional = renderNode(state,
                    generateNode(state, block.conditional));

            state.output ~= format("    test %s, %s",
                                   conditional, conditional);
            state.output ~= format("    je %s", nextBlockLabel);
        }

        // Block: run if the conditional was true
        generateStatement(state, block.block);

        // Jump out of the if structure, unless we're already at the end
        if (i == blocks.length - 1)
        {
            state.output ~= format("    jmp %s", endIfLabel);
        }
    }

    state.output ~= format("%s:", endIfLabel);
}

void generateBlock(GeneratorState state, Block block)
{
    foreach (statement; block.statements)
    {
        generateStatement(state, statement);
    }
}

void generateLocalDeclaration(GeneratorState state, LocalDeclaration st)
{
    if (st.value !is null)
    {
        generateAssignmentShared(state, st.signature.name, st.value);
    }
}

void generateAssignment(GeneratorState state, Assignment a)
{
    generateAssignmentShared(state, a.binding.name, a.value);
}

void generateAssignmentShared(GeneratorState state, string targetName,
                              Node expression)
{
    auto target = state.findLocal(targetName);

    if (target is null)
    {
        throw new Exception(format("Local %s not found", targetName));
    }

    auto localRendered = renderLocal(target);

    auto value = generateNode(state, expression);
    auto valueRendered = renderNode(state, value);

    auto sizeHint = "";
    if (target.location == Location.Stack)
    {
        sizeHint = to!string(typeToOpSize(target.type));
    }

    state.output ~= format("    mov %s%s, %s", sizeHint, localRendered,
                           valueRendered);
}

string renderImmediate(Literal literal)
{
    auto ulongLiteral = cast(ULongLiteral)literal;
    if (ulongLiteral !is null)
    {
        return to!string(ulongLiteral.value);
    }
    else
    {
        throw new Exception(
                format("Can't render literal as immediate: %s", literal));
    }
}

void generateReturn(GeneratorState state, Return r)
{
    auto value = generateNode(state, r.expression);

    // Move the return value into rax if it isn't already there
    auto binding = cast(Binding)value;
    auto literal = cast(Literal)value;
    if (binding !is null)
    {
        auto local = state.findLocal(binding.name);

        if (local is null)
        {
            throw new Exception(format("Local %s not found", binding.name));
        }

        if (local.location != Location.Register ||
            local.register != Register.RAX)
        {
            auto localRendered = renderLocal(local);
            state.output ~= format("    mov rax, %s", localRendered);
        }
    }
    else if (literal !is null)
    {
        state.output ~= format("    mov rax, %s", renderImmediate(literal));
    }
    else
    {
        throw new Exception(format("Can't return this: %s", value));
    }

    state.output ~= "    mov rsp, rbp";
    state.output ~= "    pop rbp";
    state.output ~= "    ret";
}

// Generate a Node in an expression, doing any necessary setup to get it usable
// by the calling generator.
Node generateNode(GeneratorState state, Node node)
{
    // Special handling for operators
    auto operator = cast(Operator)node;
    if (operator !is null)
    {
        auto local = generateOperator(state, operator);
        return new Binding(local.name);
    }

    // Special handling for function calls
    auto call = cast(Call)node;
    if (call !is null)
    {
        auto local = generateCall(state, call);
        return new Binding(local.name);
    }

    // Special handling for calls
    auto typeCast = cast(Cast)node;
    if (typeCast !is null)
    {
        auto local = generateCast(state, typeCast);
        return new Binding(local.name);
    }

    return node;
}

bool isPrimitiveIntegral(PrimitiveType type)
{
    return type == PrimitiveType.ULong;
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

    return isPrimitiveIntegral(local.type.primitive);
}

Local generateCast(GeneratorState state, Cast typeCast)
{
    auto unableToCast = new Exception(format("Unable to cast %s to %s",
            typeCast.target, typeCast.newType));

    if (typeCast.newType.pointer ||
        typeCast.newType.primitive == PrimitiveType.Bool)
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

Local generateCastLiteralIntegerToBool(GeneratorState state, Cast typeCast)
{
    bool foundMatch = false;
    bool castedValue;

    auto ulongLiteral = cast(ULongLiteral)typeCast.target;
    if (ulongLiteral !is null)
    {
        castedValue = ulongLiteral.value != 0;
        foundMatch = true;
    }

    if (!foundMatch)
    {
        throw new Exception(format("Can't convert %s to bool",
                            typeCast.target));
    }

    auto target = state.addTemp(new Type(PrimitiveType.Bool));

    if (castedValue)
    {
        state.output ~= format("    mov %s, 1", target.register);
    }
    else
    {
        state.output ~= format("    xor %s, %s", target.register,
                                                 target.register);
    }

    return target;
}

Local generateCastLocalIntegerToBool(GeneratorState state, Cast typeCast)
{
    auto source = renderNode(state, generateNode(state, typeCast.target));
    auto target = state.addTemp(new Type(PrimitiveType.Bool));

    state.output ~= format("    xor %s, %s", target.register, target.register);
    state.output ~= format("    cmp %s, 0", source);
    state.output ~= format("    setne %s", lowByte(target.register));

    return target;
}

Local generateOperator(GeneratorState state, Operator operator)
{
    if (operator.type == OperatorType.Plus ||
        operator.type == OperatorType.Asterisk)
    {
        return generateMathOperator(state, operator);
    }

    if (operator.type == OperatorType.Equality ||
        operator.type == OperatorType.Inequality)
    {
        return generateRelationalOperator(state, operator);
    }

    if (operator.type == OperatorType.LogicalAnd)
    {
        return generateLogicalAndOperator(state, operator);
    }

    throw new Exception(
            format("Unrecognized operator type: %s", operator.type));
}

Local generateMathOperator(GeneratorState state, Operator operator)
{
    auto left = renderNode(state, generateNode(state, operator.left));
    auto right = renderNode(state, generateNode(state, operator.right));

    auto temp = state.addTemp(new Type(PrimitiveType.ULong));
    state.output ~= format("    mov %s, %s", temp.register, left);

    switch (operator.type)
    {
        case OperatorType.Plus:
            state.output ~= format("    add %s, %s", temp.register, right);
            break;
        case OperatorType.Asterisk:
            state.output ~= format("    imul %s, %s", temp.register, right);
            break;
        default:
            throw new Exception(format("Unrecognized math operator type: %s",
                                       operator.type));
    }

    return temp;
}

Local generateRelationalOperator(GeneratorState state, Operator operator)
{
    auto left = renderNode(state, generateNode(state, operator.left));
    auto right = renderNode(state, generateNode(state, operator.right));

    auto temp = state.addTemp(new Type(PrimitiveType.Bool));
    state.output ~= format("    xor %s, %s", temp.register, temp.register);

    switch (operator.type)
    {
        case OperatorType.Equality:
            state.output ~= format("    cmp %s, %s", left, right);
            state.output ~= format("    sete %s", lowByte(temp.register));
            break;
        case OperatorType.Inequality:
            state.output ~= format("    cmp %s, %s", left, right);
            state.output ~= format("    setne %s", lowByte(temp.register));
            break;
        default:
            throw new Exception(format(
                    "Unrecognized relational operator type: %s",
                    operator.type));
    }

    return temp;
}

Local generateLogicalAndOperator(GeneratorState state, Operator operator)
{
    auto temp = state.addTemp(new Type(PrimitiveType.Bool));
    state.output ~= format("    xor %s, %s", temp.register, temp.register);

    auto endComparison = state.addLabel("end_comparison_");

    // Left operand
    auto left = renderNode(state, generateNode(state, operator.left));
    state.output ~= format("    test %s, %s", left, left);
    state.output ~= format("    je %s", endComparison);

    // Right operand
    auto right = renderNode(state, generateNode(state, operator.right));
    state.output ~= format("    test %s, %s", right, right);
    state.output ~= format("    je %s", endComparison);

    // Both were true
    state.output ~= format("    mov %s, 1", temp.register);

    // Either were false
    state.output ~= format("%s:", endComparison);

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

Local generateCall(GeneratorState state, Call call)
{
    // Generate all parameters
    Node[] params;
    for (int i = 0; i < call.parameters.length; i++)
    {
        params ~= generateNode(state, call.parameters[i]);
    }

    // Save caller-preserved registers
    // TODO: optimize
    Register[] callerPreserved = state.callerPreservedRegistersInUse();
    for (auto i = 0; i < callerPreserved.length; i++)
    {
        state.generatePush(callerPreserved[i]);
    }

    // Build list of parameters in registers and where they have to move for
    // the call to work.
    RegisterMove[] registerMoves;
    NonRegisterArg[] nonRegisterArgs;
    for (int i = 0; i < params.length; i++)
    {
        auto targetRegister = parameterRegister(i);

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
        state.output ~= format("    mov %s, %s",
                               arg.target, renderNode(state, arg.node));
    }

    // Make the actual call
    auto func = state.mod.findFunction(call.functionName);
    state.output ~= format("    call %s", renderFunctionName(func.name));

    // Save return value (in rax) to a new temp variable
    auto temp = state.addTemp(func.returnType);
    state.output ~= format("    mov %s, rax", temp.register);

    // Restore caller-preserved registers
    // TODO: optimize
    for (int i = cast(int)callerPreserved.length - 1; i >= 0; i--)
    {
        state.generatePop(callerPreserved[i]);
    }

    return temp;
}

string renderLocal(Local local)
{
    switch (local.location)
    {
        case Location.Register:
            return format("%s", local.register);
        case Location.Stack:
            return format("[rbp-%d]", local.stackOffset);
        default:
            throw new Exception(format("Unknown location %s", local.location));
    }
}

string renderNode(GeneratorState state, Node node)
{
    auto binding = cast(Binding)node;
    if (binding !is null)
    {
        auto local = state.findLocal(binding.name);
        return renderLocal(local);
    }

    auto ulongLiteral = cast(ULongLiteral)node;
    if (ulongLiteral !is null)
    {
        return format("%d", ulongLiteral.value);
    }

    auto boolLiteral = cast(BoolLiteral)node;
    if (boolLiteral !is null)
    {
        return boolLiteral.value ? "1" : "0";
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
                state.output ~= format("    mov %s, %s",
                                       chain[i], chain[i - 1]);
            }

            continue;
        }

        // Circular chain with only two nodes (optimize to use xchg)
        if (chain.length == 2)
        {
            state.output ~= format("    xchg %s, %s", chain[0], chain[1]);
            continue;
        }

        // Circular chain
        auto tempLocal = state.addTemp(new Type(PrimitiveType.ULong));
        auto temp = tempLocal.register;

        state.output ~= format("    mov %s, %s", temp, chain[$-1]);

        for (auto i = chain.length - 1; i > 0; i--)
        {
            state.output ~= format("    mov %s, %s",
                                   chain[i], chain[i - 1]);
        }

        state.output ~= format("    mov %s, %s", chain[0], temp);

        state.freeTemp(tempLocal);
    }
}
