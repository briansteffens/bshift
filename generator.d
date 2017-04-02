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

class GeneratorState
{
    string[] output;

    Local[] locals;
    Local[] temps;

    int nextTempIndex = 0;

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
    auto state = new GeneratorState();

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

// Place a parameter in the appropriate location (register etc) based on its
// index within the parameter list
void placeParameter(Local local, int index)
{
    local.location = Location.Register;

    switch (index)
    {
        case 0:
            local.register = Register.RDI;
            break;
        case 1:
            local.register = Register.RSI;
            break;
        case 2:
            local.register = Register.RDX;
            break;
        case 3:
            local.register = Register.RCX;
            break;
        case 4:
            local.register = Register.R8;
            break;
        case 5:
            local.register = Register.R9;
            break;
        default:
            throw new Exception(format(
                    "Not enough registers for parameter %d", index));
    }
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
    state.output ~= format("    ; %s", st);

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

    return node;
}

Local generateOperator(GeneratorState state, Operator operator)
{
    auto left = renderNode(state, operator.left);
    auto right = renderNode(state, operator.right);

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
