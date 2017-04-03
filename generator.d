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

enum Location
{
    Register,
}

class Local
{
    string name;
    Type type;
    Location location;
    Register register;

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

class GeneratorState
{
    Module mod;

    string[] output;

    Local[] locals;
    Local[] temps;

    int nextTempIndex = 0;

    this(Module mod)
    {
        this.mod = mod;
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
    // Function parameters need to be added to locals
    for (int i = 0; i < func.parameters.length; i++)
    {
        auto local = new Local(func.parameters[i].type,
                               func.parameters[i].name);

        placeParameter(local, i);

        state.locals ~= local;
    }

    state.output ~= format("%s:", renderFunctionName(func.name));

    for (int i = 0; i < func.statements.length; i++)
    {
        generateStatement(state, func.statements[i]);
    }

    state.locals.length = 0;
    state.temps.length = 0;
}

void generateStatement(GeneratorState state, Statement st)
{
    //state.output ~= format("    ; %s", st);

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

    throw new Exception(format("Unrecognized statement type: %s", st));
}

void generateLocalDeclaration(GeneratorState state, LocalDeclaration st)
{
    auto existing = state.findLocal(st.signature.name);

    if (existing !is null)
    {
        throw new Exception(format("Local %s already declared",
                st.signature.name));
    }

    state.addLocal(st.signature.type, st.signature.name);
}

void generateAssignment(GeneratorState state, Assignment a)
{
    auto local = state.findLocal(a.binding.name);

    if (local is null)
    {
        throw new Exception(format("Local %s not found", a.binding.name));
    }

    auto value = generateNode(state, a.value);
    auto valueRendered = renderNode(state, value);

    state.output ~= format("    mov %s, %s", local.register, valueRendered);
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
        if (local.register != Register.RAX)
        {
            state.output ~= format("    mov rax, %s", local.register);
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

    return node;
}

Local generateOperator(GeneratorState state, Operator operator)
{
    auto left = renderNode(state, generateNode(state, operator.left));
    auto right = renderNode(state, generateNode(state, operator.right));

    auto temp = state.addTemp(Type.ULong);
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
            throw new Exception(
                    format("Unrecognized operator type: %s", operator.type));
    }

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

string renderNode(GeneratorState state, Node node)
{
    auto binding = cast(Binding)node;
    if (binding !is null)
    {
        auto local = state.findLocal(binding.name);
        return format("%s", local.register);
    }

    auto ulongLiteral = cast(ULongLiteral)node;
    if (ulongLiteral !is null)
    {
        return format("%d", ulongLiteral.value);
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
        auto tempLocal = state.addTemp(Type.ULong);
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
