import std.stdio;
import std.format;

import ast;

enum Register
{
    RAX,
    RBX,
    RCX,
    RDX,
    RSI,
    RDI,
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
        for (int i = 0; i < this.locals.length; i++)
        {
            if (this.locals[i].name == name)
            {
                return this.locals[i];
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

void generateFunction(GeneratorState state, Function func)
{
    state.output ~= format("%s:", renderFunctionName(func.name));

    for (int i = 0; i < func.statements.length; i++)
    {
        generateStatement(state, func.statements[i]);
    }

    state.output ~= "    ret";
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

    throw new Exception(format("Unrecognized statement type: %s", st));
}

void generateLocalDeclaration(GeneratorState state, LocalDeclaration st)
{
    auto existing = state.findLocal(st.name);

    if (existing !is null)
    {
        throw new Exception(format("Local %s already declared", st.name));
    }

    state.locals ~= new Local(st.type, st.name);
}

void generateAssignment(GeneratorState state, Assignment a)
{
    auto local = state.findLocal(a.binding.name);

    if (local is null)
    {
        throw new Exception(format("Local %s not found", a.binding.name));
    }

    auto value = generateNode(state, a.value);

    state.output ~= format("    mov %s, %s", local.register, value.register);
}

Local generateNode(GeneratorState state, Node node)
{
    auto operator = cast(Operator)node;

    if (operator !is null)
    {
        return generateOperator(state, operator);
    }
    /*
    auto binding = cast(Binding)node;

    if (binding !is null)
    {
        generateBinding(state, binding);
        return;
    }

    auto ulongLiteral = cast(ULongLiteral)node;

    if (ulongLiteral !is null)
    {
        generateULongLiteral(state, ulongLiteral);
        return;
    }
    */
    throw new Exception(format("Node %s unrecognized", node));
}

Local generateOperator(GeneratorState state, Operator operator)
{
    auto temp = state.addTemp(Type.ULong);

    state.output ~= format(
            "    mov %s, %s", temp.register, renderNode(state, operator.left));

    auto right = renderNode(state, operator.right);

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
