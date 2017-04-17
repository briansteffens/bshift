import std.format;
import std.conv;

enum PrimitiveType
{
    U64,
    U8,
    Bool,
    Auto,
}

class Type
{
    PrimitiveType primitive;
    bool pointer;
    Node elements;

    this(PrimitiveType primitive, bool pointer=false, Node elements=null)
    {
        this.primitive = primitive;
        this.pointer = pointer;
        this.elements = elements;
    }

    override string toString()
    {
        auto ret = to!string(this.primitive);

        if (this.pointer && this.elements is null)
        {
            ret ~= "*";
        }

        if (this.elements !is null)
        {
            ret ~= format("[%s]", this.elements);
        }

        return ret;
    }

    Type clone()
    {
        // TODO: deep clone node?
        return new Type(this.primitive, this.pointer, this.elements);
    }

    bool compare(Type other)
    {
        return this.primitive == other.primitive &&
               this.pointer == other.pointer &&
               this.elements == other.elements; // TODO: value-compare node?
    }

    bool compatibleWith(Type other)
    {
        return this.compare(other) ||
               this.primitive == PrimitiveType.U64 && other.pointer ||
               this.primitive == PrimitiveType.Auto ||
               other.primitive == PrimitiveType.Auto ||
               this.pointer && other.primitive == PrimitiveType.U64;
    }

    // Makes sure the number of elements is known at compile-time
    bool isConcrete()
    {
        return this.elements is null || cast(U64Literal)this.elements !is null;
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
        case PrimitiveType.Auto:
            throw new Exception("Auto type has no size");
        default:
            throw new Exception(format("Unknown size for %s", t));
    }
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
        case "auto":
            return PrimitiveType.Auto;
        default:
            throw new Exception(format("Unrecognized type: %s", s));
    }
}

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
    Line   line;
    Type   type;
    Object parent;

    Node[] childNodes()
    {
        return [];
    }
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

enum OperatorClass
{
    Math,
    Relational,
    Logical
}

OperatorClass operatorTypeToClass(OperatorType t)
{
    switch (t)
    {
        case OperatorType.Plus:
        case OperatorType.Asterisk:
            return OperatorClass.Math;
        case OperatorType.Equality:
        case OperatorType.Inequality:
            return OperatorClass.Relational;
        case OperatorType.LogicalAnd:
            return OperatorClass.Logical;
        default:
            throw new Exception(format("Unrecognized OperatorType: %s", t));
    }
}

class Operator : Node
{
    OperatorType operatorType;
    Node left;
    Node right;

    this(Node left, OperatorType operatorType, Node right)
    {
        this.operatorType = operatorType;
        this.left = left;
        this.right = right;

        this.left.parent = this;
        this.right.parent = this;

        switch (operatorTypeToClass(operatorType))
        {
            case OperatorClass.Math:
                if (!left.type.compatibleWith(right.type))
                {
                    throw new Exception(format("Can't combine types %s and %s",
                            left.type, right.type));
                }
                this.type = left.type;
                break;

            case OperatorClass.Relational:
            case OperatorClass.Logical:
                this.type = new Type(PrimitiveType.Bool);
                break;

            default:
                throw new Exception(format("Unrecognized type %s",
                                           operatorType));
        }
    }

    override string toString()
    {
        return format("(%s %s %s)", left, operatorType, right);
    }

    override Node[] childNodes()
    {
        return [left, right];
    }
}

class Binding : Node
{
    string name;
    LocalDeclaration local;

    this(LocalDeclaration local, string name)
    {
        this.local = local;
        this.name = name;

        this.type = local.signature.type;
    }

    override string toString()
    {
        return this.local.signature.name;
    }
}

abstract class Literal : Node
{
    this(PrimitiveType type)
    {
        this.type = new Type(type);
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
    FunctionSignature targetSignature;

    this(string moduleName, string functionName, Node[] parameters)
    {
        // TODO: set return type
        this.moduleName = moduleName;
        this.functionName = functionName;
        this.parameters = parameters;

        foreach (param; this.parameters)
        {
            param.parent = this;
        }
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

    override Node[] childNodes()
    {
        return this.parameters;
    }
}

class Cast : Node
{
    Node target;

    this(Type newType, Node target)
    {
        this.target = target;
        this.target.parent = this;

        this.type = newType;
    }

    override string toString()
    {
        return format("(%s)%s", this.type, this.target);
    }

    override Node[] childNodes()
    {
        return [this.target];
    }
}

class Reference : Node
{
    Node source;

    this(Node source)
    {
        this.source = source;
        this.source.parent = this;

        this.type = source.type.clone();
        this.type.pointer = true;
    }

    override string toString()
    {
        return format("&%s", this.source);
    }

    override Node[] childNodes()
    {
        return [this.source];
    }
}

class Dereference : Node
{
    Node source;

    this(Node source)
    {
        this.source = source;
        this.source.parent = this;

        this.type = source.type.clone();
        this.type.pointer = false;
    }

    override string toString()
    {
        return format("*%s", this.source);
    }

    override Node[] childNodes()
    {
        return [this.source];
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
    Line   line;
    Object parent;

    this(Line line)
    {
        this.line = line;
    }

    LocalDeclaration[] declarations()
    {
        return [];
    }

    Statement[] childStatements()
    {
        return [];
    }

    Node[] childNodes()
    {
        return [];
    }
}

class Indexer : Node
{
    Node source;
    Node index;

    this(Node source, Node index)
    {
        this.source = source;
        this.index = index;

        this.source.parent = this;
        this.source.parent = this;

        this.type = this.source.type.clone();
        this.type.pointer = false;
        this.type.elements = null;
    }

    override string toString()
    {
        return format("%s[%s]", this.source, this.index);
    }

    override Node[] childNodes()
    {
        return [this.source, this.index];
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

        if (this.value !is null)
        {
            this.value.parent = this;
        }

        if (this.signature.type.primitive == PrimitiveType.Auto)
        {
            if (this.value is null)
            {
                throw new Exception(format("Can't infer type for %s",
                        signature));
            }

            this.signature.type = value.type;
        }
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

    override Node[] childNodes()
    {
        if (this.value is null)
        {
            return [];
        }
        else
        {
            return [this.value];
        }
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

        this.lvalue.parent = this;
        this.value.parent = this;
    }

    override string toString()
    {
        return format("%s = %s", this.lvalue, this.value);
    }

    override Node[] childNodes()
    {
        return [this.lvalue, this.value];
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

        this.conditional.parent = this;
        this.block.parent = this;
    }

    override string toString()
    {
        return format("(%s)\n%s", this.conditional, this.block);
    }

    override LocalDeclaration[] declarations()
    {
        return block.declarations();
    }

    override Statement[] childStatements()
    {
        return [this.block];
    }

    override Node[] childNodes()
    {
        return [this.conditional];
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

        this.ifBlock.parent = this;
        foreach (elseIf; this.elseIfBlocks)
        {
            elseIf.parent = this;
        }
        this.elseBlock.parent = this;
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

    override Statement[] childStatements()
    {
        Statement[] ret = [this.ifBlock];

        ret ~= this.elseIfBlocks;

        if (this.elseBlock !is null)
        {
            ret ~= this.elseBlock;
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

        this.expression.parent = this;
    }

    override string toString()
    {
        return format("return %s", this.expression);
    }

    override Node[] childNodes()
    {
        if (this.expression is null)
        {
            return [];
        }
        else
        {
            return [this.expression];
        }
    }
}

class Block : Statement
{
    Statement[] statements;

    this(Statement[] statements)
    {
        super(null);

        this.statements = statements;

        foreach (st; this.statements)
        {
            st.parent = this;
        }
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

    override Statement[] childStatements()
    {
        return this.statements;
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
}

class Function : FunctionSignature
{
    Module mod;
    Block block;

    this(Type returnType, string name, TypeSignature[] parameters, Block block)
    {
        super(returnType, name, parameters);

        this.block = block;

        this.block.parent = this;
    }

    override string toString()
    {
        return format("%s\n%s", super.toString(), this.block);
    }
}

class Import
{
    string name;
    FunctionSignature[] functions;

    this(string name, FunctionSignature[] functions)
    {
        this.name = name;
        this.functions = functions;
    }

    override string toString()
    {
        auto ret = format("import %s", this.name);

        foreach (func; this.functions)
        {
            ret ~= format("    %s", func);
        }

        return ret;
    }

    FunctionSignature findFunction(string name)
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
}

class Module
{
    string name;
    Import[] imports;
    Function[] functions;
    FunctionSignature[] externs;

    this(string name, Import[] imports, Function[] functions,
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
        return imp.findFunction(call.functionName);
    }

    Import findImport(string name)
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
