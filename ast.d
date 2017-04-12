import std.format;
import std.conv;

class Line
{
    int number;
    string source;

    this(int number, string source)
    {
        this.number = number;
        this.source = source;
    }
}

abstract class Node
{
    Line line;
}

enum OperatorType
{
    Plus,
    Asterisk,
    Equality,
    Inequality,
    LogicalAnd,
}

OperatorType parseOperatorType(string input)
{
    switch (input)
    {
        case "+":
            return OperatorType.Plus;
        case "*":
            return OperatorType.Asterisk;
        case "==":
            return OperatorType.Equality;
        case "!=":
            return OperatorType.Inequality;
        case "&&":
            return OperatorType.LogicalAnd;
        default:
            throw new Exception(
                    format("Unrecognized OperatorType: %s", input));
    }
}

class Operator : Node
{
    OperatorType type;
    Node left;
    Node right;

    this(Node left, OperatorType type, Node right)
    {
        this.type = type;
        this.left = left;
        this.right = right;
    }

    override string toString()
    {
        return format("(%s %s %s)", left, type, right);
    }
}

class Binding : Node
{
    string name;

    this(string name)
    {
        this.name = name;
    }

    override string toString()
    {
        return this.name;
    }
}

enum PrimitiveType
{
    U64,
    U8,
    Bool,
}

class Type
{
    PrimitiveType primitive;
    bool pointer;

    this(PrimitiveType primitive, bool pointer = false)
    {
        this.primitive = primitive;
        this.pointer = pointer;
    }

    override string toString()
    {
        auto ret = to!string(this.primitive);

        if (this.pointer)
        {
            ret ~= "*";
        }

        return ret;
    }

    Type clone()
    {
        return new Type(this.primitive, this.pointer);
    }

    bool compare(Type other)
    {
        return this.primitive == other.primitive &&
               this.pointer == other.pointer;
    }

    bool compatibleWith(Type other)
    {
        return this.compare(other) ||
               this.primitive == PrimitiveType.U64 && other.pointer ||
               this.pointer && other.primitive == PrimitiveType.U64;
    }
}

int primitiveSize(PrimitiveType t)
{
    switch (t)
    {
        case PrimitiveType.U64:
            return 8;
        case PrimitiveType.Bool:
            return 1;
        case PrimitiveType.U8:
            return 1;
        default:
            throw new Exception(format("Unknown size for %s", t));
    }
}

int typeSize(Type t)
{
    if (t.pointer)
    {
        return 8;
    }

    return primitiveSize(t.primitive);
}

PrimitiveType parsePrimitive(string s)
{
    switch (s)
    {
        case "u64":
            return PrimitiveType.U64;
        case "bool":
            return PrimitiveType.Bool;
        case "u8":
            return PrimitiveType.U8;
        default:
            throw new Exception(format("Unrecognized type: %s", s));
    }
}

abstract class Literal : Node
{
    PrimitiveType type;

    this(PrimitiveType type)
    {
        this.type = type;
    }
}

class U64Literal : Literal
{
    ulong value;

    this(ulong value)
    {
        super(PrimitiveType.U64);
        this.value = value;
    }

    override string toString()
    {
        return to!string(this.value);
    }
}

class BoolLiteral : Literal
{
    bool value;

    this(bool value)
    {
        super(PrimitiveType.Bool);
        this.value = value;
    }

    override string toString()
    {
        return to!string(this.value);
    }
}

class Call : Node
{
    string moduleName;
    string functionName;
    Node[] parameters;

    this(string moduleName, string functionName, Node[] parameters)
    {
        this.moduleName = moduleName;
        this.functionName = functionName;
        this.parameters = parameters;
    }

    override string toString()
    {
        if (this.moduleName is null)
        {
            return format("%s(%s)", this.functionName, this.parameters);
        }
        else
        {
            return format("%s::%s(%s)", this.moduleName, this.functionName,
                          this.parameters);
        }
    }
}

class Cast : Node
{
    Type newType;
    Node target;

    this(Type newType, Node target)
    {
        this.newType = newType;
        this.target = target;
    }

    override string toString()
    {
        return format("(%s)%s", this.newType, this.target);
    }
}

class Reference : Node
{
    Node source;

    this(Node source)
    {
        this.source = source;
    }

    override string toString()
    {
        return format("&%s", this.source);
    }
}

class Dereference : Node
{
    Node source;

    this(Node source)
    {
        this.source = source;
    }

    override string toString()
    {
        return format("*%s", this.source);
    }
}

class TypeSignature
{
    Type type;
    string name;

    this(Type type, string name)
    {
        this.type = type;
        this.name = name;
    }

    override string toString()
    {
        return format("%s %s", this.type, this.name);
    }
}

abstract class Statement
{
    Line line;

    this(Line line)
    {
        this.line = line;
    }

    LocalDeclaration[] declarations()
    {
        return [];
    }
}

class LocalDeclaration : Statement
{
    TypeSignature signature;
    Node value;

    this(Line line, TypeSignature signature, Node value)
    {
        super(line);

        this.signature = signature;
        this.value = value;
    }

    override string toString()
    {
        auto ret = format("Local %s", this.signature);

        if (this.value !is null)
        {
            ret ~= format(" = %s", this.value);
        }

        return ret;
    }

    override LocalDeclaration[] declarations()
    {
        return [this];
    }
}

class Assignment : Statement
{
    Node lvalue;
    Node value;

    this(Line line, Node lvalue, Node value)
    {
        super(line);

        this.lvalue = lvalue;
        this.value = value;
    }

    override string toString()
    {
        return format("%s = %s", this.lvalue, this.value);
    }
}

class ConditionalBlock : Statement
{
    Node conditional;
    Statement block;

    this(Node conditional, Statement block)
    {
        super(null);

        this.conditional = conditional;
        this.block = block;
    }

    override string toString()
    {
        return format("(%s)\n%s", this.conditional, this.block);
    }

    override LocalDeclaration[] declarations()
    {
        return block.declarations();
    }
}

class While : ConditionalBlock
{
    this(Node conditional, Statement block)
    {
        super(conditional, block);
    }

    override string toString()
    {
        return format("while\n%s", super.toString());
    }
}

class If : Statement
{
    ConditionalBlock ifBlock;
    ConditionalBlock[] elseIfBlocks;
    Statement elseBlock;

    this(ConditionalBlock ifBlock, ConditionalBlock[] elseIfBlocks,
         Statement elseBlock)
    {
        super(null);

        this.ifBlock = ifBlock;
        this.elseIfBlocks = elseIfBlocks;
        this.elseBlock = elseBlock;
    }

    override string toString()
    {
        auto ret = format("if (%s)\n%s\n", this.ifBlock.conditional,
                          this.ifBlock.block);

        foreach (elseIf; this.elseIfBlocks)
        {
            ret ~= format("else if (%s)\n%s\n", elseIf.conditional,
                    elseIf.block);
        }

        if (this.elseBlock !is null)
        {
            ret ~= format("else\n%s\n", this.elseBlock);
        }

        return ret;
    }

    override LocalDeclaration[] declarations()
    {
        LocalDeclaration[] ret = this.ifBlock.declarations();

        foreach (elseIf; this.elseIfBlocks)
        {
            ret ~= elseIf.declarations();
        }

        if (this.elseBlock !is null)
        {
            ret ~= this.elseBlock.declarations();
        }

        return ret;
    }
}

class Return : Statement
{
    Node expression;

    this(Line line, Node expression)
    {
        super(line);

        this.expression = expression;
    }

    override string toString()
    {
        return format("return %s", this.expression);
    }
}

class Block : Statement
{
    Statement[] statements;

    this(Statement[] statements)
    {
        super(null);

        this.statements = statements;
    }

    override string toString()
    {
        auto ret = "{\n";

        foreach (statement; this.statements)
        {
            ret ~= format("    %s\n", statement);
        }

        return ret ~ "}\n";
    }

    override LocalDeclaration[] declarations()
    {
        LocalDeclaration[] ret;

        foreach (statement; this.statements)
        {
            ret ~= statement.declarations();
        }

        return ret;
    }
}

class Definition
{
}

class FunctionSignature : Definition
{
    Type returnType;
    string name;
    TypeSignature[] parameters;

    this(Type returnType, string name, TypeSignature[] parameters)
    {
        this.returnType = returnType;
        this.name = name;
        this.parameters = parameters;
    }

    override string toString()
    {
        auto params = "";

        foreach (param; this.parameters)
        {
            if (params != "")
            {
                params ~= ", ";
            }

            params ~= param.toString();
        }

        return format("%s %s(%s)", this.returnType, this.name, params);
    }

    string renderName()
    {
        return this.name;
    }
}

class Function : FunctionSignature
{
    Module mod;
    Block block;

    this(Type returnType, string name, TypeSignature[] parameters, Block block)
    {
        super(returnType, name, parameters);
        this.block = block;
    }

    override string toString()
    {
        return format("%s\n%s", super.toString(), this.block);
    }

    override string renderName()
    {
        return format("function_%s_%s", this.mod.name, this.name);
    }
}

class Module
{
    string name;
    Module[] imports;
    Function[] functions;
    FunctionSignature[] externs;

    this(string name, Module[] imports, Function[] functions,
         FunctionSignature[] externs)
    {
        this.name = name;
        this.imports = imports;
        this.functions = functions;
        this.externs = externs;
    }

    override string toString()
    {
        auto ret = "";

        foreach (imp; this.imports)
        {
            ret ~= imp.toString() ~ "\n";
        }

        foreach (ext; this.externs)
        {
            ret ~= ext.toString() ~ "\n";
        }

        foreach (func; this.functions)
        {
            ret ~= func.toString() ~ "\n";
        }

        return ret;
    }

    bool functionExists(string name)
    {
        foreach (func; this.functions)
        {
            if (func.name == name)
            {
                return true;
            }
        }

        return false;
    }

    Function findFunction(string name)
    {
        foreach (func; this.functions)
        {
            if (func.name == name)
            {
                return func;
            }
        }

        throw new Exception(format("Function %s not found", name));
    }

    FunctionSignature findFunction(Call call)
    {
        // Search the current module
        if (call.moduleName is null)
        {
            foreach (func; this.functions)
            {
                if (func.name == call.functionName)
                {
                    return func;
                }
            }

            // Search for externs defined in the current module
            foreach (ext; this.externs)
            {
                if (ext.name == call.functionName)
                {
                    return ext;
                }
            }

            throw new Exception(format("Function %s not found", name));
        }

        // Search imported modules
        auto imp = this.findImport(call.moduleName);
        return imp.findFunction(call);
    }

    Module findImport(string name)
    {
        foreach (imp; this.imports)
        {
            if (imp.name == name)
            {
                return imp;
            }
        }

        throw new Exception(format("Import %s not found", name));
    }
}
