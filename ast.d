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
}

OperatorType parseOperatorType(string input)
{
    switch (input)
    {
        case "+":
            return OperatorType.Plus;
        case "*":
            return OperatorType.Asterisk;
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

enum Type
{
    ULong,
    Bool,
}

Type parseType(string s)
{
    switch (s)
    {
        case "ulong":
            return Type.ULong;
        case "bool":
            return Type.Bool;
        default:
            throw new Exception(format("Unrecognized type: %s", s));
    }
}

abstract class Literal : Node
{
    Type type;

    this(Type type)
    {
        this.type = type;
    }
}

class ULongLiteral : Literal
{
    ulong value;

    this(ulong value)
    {
        super(Type.ULong);
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
        super(Type.Bool);
        this.value = value;
    }

    override string toString()
    {
        return to!string(this.value);
    }
}

abstract class Statement
{
    Line line;

    this(Line line)
    {
        this.line = line;
    }
}

class Call : Node
{
    string functionName;
    Node[] parameters;

    this(string functionName, Node[] parameters)
    {
        this.functionName = functionName;
        this.parameters = parameters;
    }

    override string toString()
    {
        return format("%s(%s)", this.functionName, this.parameters);
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

class LocalDeclaration : Statement
{
    TypeSignature signature;

    this(Line line, TypeSignature signature)
    {
        super(line);

        this.signature = signature;
    }

    override string toString()
    {
        return format("Local %s", this.signature);
    }
}

class Assignment : Statement
{
    Binding binding;
    Node value;

    this(Line line, Binding binding, Node value)
    {
        super(line);

        this.binding = binding;
        this.value = value;
    }

    override string toString()
    {
        return format("%s = %s", this.binding, this.value);
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

class Definition
{
}

class Function : Definition
{
    Type returnType;
    string name;
    TypeSignature[] parameters;
    Statement[] statements;

    this(Type returnType, string name, TypeSignature[] parameters,
         Statement[] statements)
    {
        this.returnType = returnType;
        this.name = name;
        this.parameters = parameters;
        this.statements = statements;
    }

    override string toString()
    {
        auto params = "";

        for (int i = 0; i < this.parameters.length; i++)
        {
            if (i > 0)
            {
                params ~= ", ";
            }

            params ~= this.parameters[i].toString();
        }

        auto ret = format("%s %s(%s) {\n", this.returnType, this.name, params);

        for (int i = 0; i < this.statements.length; i++)
        {
            ret ~= format("    %s\n", this.statements[i]);
        }

        return ret ~ "}\n";
    }
}

class Module
{
    Function[] functions;

    this(Function[] functions)
    {
        this.functions = functions;
    }

    override string toString()
    {
        auto ret = "";

        for (int i = 0; i < this.functions.length; i++)
        {
            ret ~= this.functions[i].toString();
            ret ~= "\n";
        }

        return ret;
    }

    Function findFunction(string name)
    {
        for (int i = 0; i < this.functions.length; i++)
        {
            if (this.functions[i].name == name)
            {
                return this.functions[i];
            }
        }

        throw new Exception(format("Function %s not found", name));
    }
}
