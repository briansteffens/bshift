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
        return format("%s %s %s", left, type, right);
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
}

Type parseType(string s)
{
    switch (s)
    {
        case "ulong":
            return Type.ULong;
        default:
            throw new Exception(format("Unrecognized type: %s", s));
    }
}

abstract class Literal : Node
{
}

class ULongLiteral : Literal
{
    ulong value;

    this(ulong value)
    {
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

class LocalDeclaration : Statement
{
    Type type;
    string name;

    this(Line line, Type type, string name)
    {
        super(line);

        this.type = type;
        this.name = name;
    }

    override string toString()
    {
        return format("%s %s", this.type, this.name);
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
    Statement[] statements;

    this(Type returnType, string name, Statement[] statements)
    {
        this.returnType = returnType;
        this.name = name;
        this.statements = statements;
    }

    override string toString()
    {
        auto ret = format("%s %s() {\n", this.returnType, this.name);

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
}
