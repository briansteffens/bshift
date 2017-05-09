import std.stdio;
import std.format;
import std.conv;

enum Primitive
{
    U64,
    U8,
    Bool,
    Auto,
}

abstract class Type
{
    bool pointer;
    Node elements;

    this(bool pointer=false, Node elements=null)
    {
        this.pointer = pointer;
        this.elements = elements;
    }

    abstract string baseTypeToString();

    override string toString()
    {
        auto ret = this.baseTypeToString();

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

    abstract Type clone();
    abstract bool compare(Type other);
    abstract bool compatibleWith(Type other);
    abstract int baseTypeSize();

    // Makes sure the number of elements is known at compile-time
    bool isConcrete()
    {
        return this.elements is null || cast(U64Literal)this.elements !is null;
    }

    bool isComplete()
    {
        return cast(IncompleteType)this is null;
    }

    bool isPrimitive()
    {
        return cast(PrimitiveType)this !is null;
    }

    bool isPrimitive(Primitive p)
    {
        return this.isPrimitive() && (cast(PrimitiveType)this).primitive == p;
    }

    bool isStruct()
    {
        return cast(StructType)this !is null;
    }

    bool isStruct(Struct s)
    {
        return this.isStruct() && (cast(StructType)this).struct_ == s;
    }
}

// A type where the base type (u64 or a struct name) is still a string and has
// not yet been parsed or linked up to the AST.
class IncompleteType : Type
{
    string name;

    this(string name, bool pointer=false, Node elements=null)
    {
        super(pointer=pointer, elements=elements);
        this.name = name;
    }

    override string baseTypeToString()
    {
        return this.name;
    }

    override string toString()
    {
        return "Incomplete(" ~ super.toString() ~ ")";
    }

    override Type clone()
    {
        return new IncompleteType(this.name, pointer=this.pointer,
                elements=this.elements);
    }

    override bool compare(Type other)
    {
        throw new Exception("Cannot compare incomplete types");
    }

    override bool compatibleWith(Type other)
    {
        throw new Exception(
                "Incomplete types are not compatible with anything");
    }

    override int baseTypeSize()
    {
        throw new Exception(format(
                "Incomplete type (%s) has no known base type size", this));
    }
}

class StructType : Type
{
    Struct struct_;

    this(Struct struct_, bool pointer=false, Node elements=null)
    {
        super(pointer=pointer, elements=elements);
        this.struct_ = struct_;
    }

    override string baseTypeToString()
    {
        return to!string(this.struct_.name);
    }

    override Type clone()
    {
        return new StructType(this.struct_, pointer=this.pointer,
                elements=this.elements);
    }

    override bool compare(Type other)
    {
        return other.isStruct(this.struct_);
    }

    override bool compatibleWith(Type other)
    {
        throw new Exception("Structs aren't compatible with other types");
    }

    override int baseTypeSize()
    {
        if (this.pointer)
        {
            return 8;
        }

        auto ret = 0;

        foreach (member; this.struct_.members)
        {
            ret += member.type.baseTypeSize();
        }

        return ret;
    }
}

class VoidType : Type
{
    this()
    {
        super(pointer=false, elements=null);
    }

    override string baseTypeToString()
    {
        return "void";
    }

    override Type clone()
    {
        return new VoidType();
    }

    override bool compare(Type other)
    {
        return cast(VoidType)other !is null;
    }

    override bool compatibleWith(Type other)
    {
        return this.compare(other);
    }

    override int baseTypeSize()
    {
        throw new Exception("void has no size");
    }
}

class PrimitiveType : Type
{
    Primitive primitive;

    this(Primitive primitive, bool pointer=false, Node elements=null)
    {
        super(pointer=pointer, elements=elements);
        this.primitive = primitive;
    }

    override string baseTypeToString()
    {
        return to!string(this.primitive);
    }

    override Type clone()
    {
        return new PrimitiveType(this.primitive, pointer=this.pointer,
                elements=this.elements);
    }

    override bool compare(Type other)
    {
        if (!this.isComplete() || !other.isComplete())
        {
            throw new Exception("Can't compare incomplete types");
        }

        auto otherPrimitive = cast(PrimitiveType)other;
        if (otherPrimitive is null)
        {
            return false;
        }

        return this.primitive == otherPrimitive.primitive &&
               this.pointer == otherPrimitive.pointer &&
               this.elements == otherPrimitive.elements; // TODO: value-compare
                                                         // node?
    }

    // TODO: maybe compare should depend on compatibleWith instead of the
    // other way around.
    override bool compatibleWith(Type other)
    {
        if (!this.isComplete() || !other.isComplete())
        {
            throw new Exception("Can't compare incomplete types");
        }

        auto otherPrimitive = cast(PrimitiveType)other;
        if (otherPrimitive is null)
        {
            return false;
        }

        return this.compare(other) ||
               this.primitive == Primitive.U64 && other.pointer ||
               this.primitive == Primitive.Auto ||
               otherPrimitive.primitive == Primitive.Auto ||
               this.pointer && otherPrimitive.primitive == Primitive.U64;
    }

    override int baseTypeSize()
    {
        if (this.pointer && this.elements is null)
        {
            return 8;
        }

        return primitiveSize(this.primitive);
    }
}

int primitiveSize(Primitive t)
{
    switch (t)
    {
        case Primitive.U64:
            return 8;
        case Primitive.Bool:
            return 1;
        case Primitive.U8:
            return 1;
        case Primitive.Auto:
            throw new Exception("Auto type has no size");
        default:
            throw new Exception(format("Unknown size for %s", t));
    }
}

Primitive parsePrimitive(string s)
{
    switch (s)
    {
        case "u64":
            return Primitive.U64;
        case "bool":
            return Primitive.Bool;
        case "u8":
            return Primitive.U8;
        case "auto":
            return Primitive.Auto;
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

interface InsideFunction
{
    FunctionSignature containingFunction();
}

abstract class Node : InsideFunction
{
    Line           line;
    Type           type;
    InsideFunction parent;

    Node[] childNodes()
    {
        return [];
    }

    // Called when a descendant node has been changed or its type has been
    // completed to trigger a walk up the tree changing any ancestor types.
    void retype()
    {
        auto parentNode = cast(Node)this.parent;
        if (parentNode !is null)
        {
            parentNode.retype();
        }
    }

    FunctionSignature containingFunction()
    {
        if (this.parent is null)
        {
            return null;
        }

        return this.parent.containingFunction();
    }
}

enum OperatorType
{
    Plus,
    Minus,
    Divide,
    Asterisk,
    Modulo,
    Equality,
    Inequality,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    LogicalAnd,
    DotAccessor,
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
        case OperatorType.Minus:
        case OperatorType.Asterisk:
        case OperatorType.Divide:
        case OperatorType.Modulo:
            return OperatorClass.Math;
        case OperatorType.Equality:
        case OperatorType.Inequality:
        case OperatorType.GreaterThan:
        case OperatorType.GreaterThanOrEqual:
        case OperatorType.LessThan:
        case OperatorType.LessThanOrEqual:
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

        this.retype();
    }

    override void retype()
    {
        if (this.left.type is null || this.right.type is null ||
            !this.left.type.isComplete() || !this.right.type.isComplete())
        {
            return;
        }

        switch (operatorTypeToClass(this.operatorType))
        {
            case OperatorClass.Math:
                if (!this.left.type.compatibleWith(this.right.type))
                {
                    throw new Exception(format("Can't combine types %s and %s",
                            this.left.type, this.right.type));
                }
                this.type = this.left.type;
                break;

            case OperatorClass.Relational:
            case OperatorClass.Logical:
                this.type = new PrimitiveType(Primitive.Bool);
                break;

            default:
                throw new Exception(format("Unrecognized type %s",
                                           operatorType));
        }

        super.retype();
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
    TypeSignature local;

    this(TypeSignature local, string name)
    {
        this.local = local;
        this.name = name;

        this.retype();
    }

    override void retype()
    {
        if (this.local is null)
        {
            return;
        }

        this.type = this.local.type;

        super.retype();
    }

    override string toString()
    {
        return this.name;
    }
}

abstract class Literal : Node
{
    this(Primitive type)
    {
        this.type = new PrimitiveType(type);
    }
}

class U64Literal : Literal
{
    ulong value;

    this(ulong value)
    {
        super(Primitive.U64);
        this.value = value;
    }

    override string toString()
    {
        return to!string(this.value);
    }
}

class U8Literal : Literal
{
    ubyte value;

    this(ubyte value)
    {
        super(Primitive.U8);
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
        super(Primitive.Bool);
        this.value = value;
    }

    override string toString()
    {
        return to!string(this.value);
    }
}

class StringLiteral : Literal
{
    string value;

    this(string value)
    {
        super(Primitive.U64);
        this.value = value;
    }

    override string toString()
    {
        return format("\"%s\"", this.value);
    }
}

class SizeOf : Node
{
    Type argument;

    this(Type argument)
    {
        this.argument = argument;
        this.type = new PrimitiveType(Primitive.U64);
    }

    override string toString()
    {
        return format("sizeof(%s)", this.argument);
    }
}

class Call : Node
{
    // TODO: doesn't apply to MethodCall
    string moduleName;
    string functionName;
    Node[] parameters;
    FunctionSignature targetSignature;

    this(string moduleName, string functionName, Node[] parameters)
    {
        this.moduleName = moduleName;
        this.functionName = functionName;
        this.parameters = parameters;

        foreach (param; this.parameters)
        {
            param.parent = this;
        }
    }

    override void retype()
    {
        if (this.targetSignature is null)
        {
            return;
        }

        this.type = this.targetSignature.returnType;

        super.retype();
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

class MethodCall : Call
{
    Node container;

    this(Node container, string functionName, Node[] parameters)
    {
        super(null, functionName, parameters);

        this.container = container;

        this.container.parent = this;
    }

    override string toString()
    {
        return format("%s.%s", this.container, super.toString());
    }

    override Node[] childNodes()
    {
        auto ret = super.childNodes();

        // Add container if it isn't already a parameter
        bool found = false;
        foreach (r; ret)
        {
            if (r == this.container)
            {
                found = true;
                break;
            }
        }
        if (!found)
        {
            ret ~= this.container;
        }

        return ret;
    }

    @property MethodSignature methodSignature()
    {
        if (this.targetSignature is null)
        {
            return null;
        }

        auto ret = cast(MethodSignature)this.targetSignature;
        if (ret is null)
        {
            throw new Exception(format(
                    "MethodCall has a bad target %s", this));
        }

        return ret;
    }

    @property MethodSignature methodSignature(MethodSignature sig)
    {
        this.targetSignature = sig;
        return this.methodSignature;
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

        this.retype();
    }

    override void retype()
    {
        if (this.source.type is null)
        {
            return;
        }

        this.type = this.source.type.clone();
        this.type.pointer = true;

        super.retype();
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

        this.retype();
    }

    override void retype()
    {
        if (this.source.type is null)
        {
            return;
        }

        this.type = this.source.type.clone();
        this.type.pointer = false;

        super.retype();
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

class DotAccessor : Node
{
    Node container;

    string memberName;
    TypeSignature member;

    this(Node container, string memberName)
    {
        this.container = container;
        this.memberName = memberName;

        this.container.parent = this;

        this.retype();
    }

    override string toString()
    {
        return format("(%s.%s)", container, memberName);
    }

    override void retype()
    {
        if (this.container.type is null || !this.container.type.isComplete())
        {
            return;
        }

        auto containerStruct = cast(StructType)this.container.type;
        if (containerStruct is null)
        {
            throw new Exception("Dot accessor with non-struct container");
        }

        // Look up struct member
        this.member = null;

        foreach (m; containerStruct.struct_.members)
        {
            if (m.name == this.memberName)
            {
                this.member = m;
            }
        }

        if (this.member is null)
        {
            throw new Exception(format("Can't find member %s",
                    this.memberName));
        }

        this.type = this.member.type;

        super.retype();
    }

    override Node[] childNodes()
    {
        return [this.container];
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

class Struct
{
    string name;
    TypeSignature[] members;
    MethodSignature[] methods;

    this(string name, TypeSignature[] members)
    {
        this.name = name;
        this.members = members;
    }

    override string toString()
    {
        auto ret = format("struct %s {\n", this.name);

        foreach (member; this.members)
        {
            ret ~= format("    %s;\n", member);
        }

        return ret ~ "}\n";
    }
}

abstract class StatementBase : InsideFunction
{
    Line           line;
    InsideFunction parent;

    this(Line line)
    {
        this.line = line;
    }

    LocalDeclaration[] declarations()
    {
        return [];
    }

    StatementBase[] childStatements()
    {
        return [];
    }

    Node[] childNodes()
    {
        return [];
    }

    FunctionSignature containingFunction()
    {
        if (parent is null)
        {
            return null;
        }

        return parent.containingFunction();
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

        this.retype();
    }

    override void retype()
    {
        if (this.source.type is null)
        {
            return;
        }

        this.type = this.source.type.clone();
        this.type.pointer = false;
        this.type.elements = null;

        super.retype();
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

class Statement : StatementBase
{
    Node expression;

    this(Line line, Node expression)
    {
        super(line);

        this.expression = expression;

        if (this.expression !is null)
        {
            this.expression.parent = this;
        }
    }

    override string toString()
    {
        return format("Statement %s", this.expression);
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

class LocalDeclaration : StatementBase
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

class Assignment : StatementBase
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

class Defer : StatementBase
{
    StatementBase statement;

    this(Line line, StatementBase statement)
    {
        super(line);

        this.statement = statement;

        this.statement.parent = this;
    }

    override string toString()
    {
        return format("defer %s", this.statement);
    }

    override StatementBase[] childStatements()
    {
        return [this.statement];
    }
}

class ConditionalBlock : StatementBase
{
    Node conditional;
    StatementBase block;

    this(Node conditional, StatementBase block)
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

    override StatementBase[] childStatements()
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
    this(Node conditional, StatementBase block)
    {
        super(conditional, block);
    }

    override string toString()
    {
        return format("while\n%s", super.toString());
    }
}

class If : StatementBase
{
    ConditionalBlock ifBlock;
    ConditionalBlock[] elseIfBlocks;
    StatementBase elseBlock;

    this(ConditionalBlock ifBlock, ConditionalBlock[] elseIfBlocks,
         StatementBase elseBlock)
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

        if (this.elseBlock !is null)
        {
            this.elseBlock.parent = this;
        }
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

    override StatementBase[] childStatements()
    {
        StatementBase[] ret = [this.ifBlock];

        ret ~= this.elseIfBlocks;

        if (this.elseBlock !is null)
        {
            ret ~= this.elseBlock;
        }

        return ret;
    }
}

class Return : StatementBase
{
    Node expression;

    this(Line line, Node expression)
    {
        super(line);

        this.expression = expression;

        if (this.expression !is null)
        {
            this.expression.parent = this;
        }
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

class Block : StatementBase
{
    StatementBase[] statements;

    this(StatementBase[] statements)
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

    override StatementBase[] childStatements()
    {
        return this.statements;
    }
}

class FunctionSignature
{
    Module mod;
    Type returnType;
    string name;
    TypeSignature[] parameters;
    bool variadic;

    this(Type returnType, string name, TypeSignature[] parameters,
         bool variadic)
    {
        this.returnType = returnType;
        this.name = name;
        this.parameters = parameters;
        this.variadic = variadic;
    }

    string fullName()
    {
        return this.name;
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

        if (this.variadic)
        {
            if (params != "")
            {
                params ~= ", ";
            }

            params ~= "...";
        }

        return format("%s %s(%s)", this.returnType, this.fullName(), params);
    }
}

class MethodSignature : FunctionSignature
{
    Type containerType;

    this(Type returnType, Type containerType, string methodName,
         TypeSignature[] parameters, bool variadic)
    {
        super(returnType, methodName, parameters, variadic);
        this.containerType = containerType;
    }

    override string fullName()
    {
        return format("%s::%s", this.containerType, this.name);
    }

    bool matchMethodCall(MethodCall call)
    {
        return call.container.type.compare(this.containerType) &&
               call.functionName == this.name;
    }
}

class Function : InsideFunction
{
    FunctionSignature signature;
    Block block;

    this(FunctionSignature signature, Block block)
    {
        this.signature = signature;
        this.block = block;

        this.block.parent = this;
    }

    override string toString()
    {
        return format("%s\n%s", this.signature, this.block);
    }

    FunctionSignature containingFunction()
    {
        return this.signature;
    }
}

class Method : Function
{
    this(MethodSignature signature, Block block)
    {
        super(signature, block);
    }

    @property MethodSignature methodSignature()
    {
        if (this.signature is null)
        {
            return null;
        }

        auto ret = cast(MethodSignature)this.signature;
        if (ret is null)
        {
            throw new Exception(format("Method has a bad signature %s", this));
        }

        return ret;
    }

    @property MethodSignature methodSignature(MethodSignature sig)
    {
        this.signature = sig;
        return this.methodSignature;
    }
}

class Import
{
    string filename;
    string name;
    FunctionSignature[] functions;
    Struct[] structs;

    this(string filename, string name, FunctionSignature[] functions)
    {
        this.filename = filename;
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

        foreach (s; this.structs)
        {
            ret ~= format("    %s", s);
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

class Global
{
    TypeSignature signature;
    Node value;

    this(TypeSignature signature, Node value)
    {
        this.signature = signature;
        this.value = value;
    }

    override string toString()
    {
        if (this.value is null)
        {
            return this.signature.toString();
        }
        else
        {
            return format("%s = %s", this.signature, this.value);
        }
    }
}

class Module
{
    string name;
    Import[] imports;
    Struct[] structs;
    Function[] functions;
    Global[] globals;
    FunctionSignature[] externs;

    this(string name, Import[] imports, Struct[] structs, Function[] functions,
         Global[] globals, FunctionSignature[] externs)
    {
        this.name = name;
        this.imports = imports;
        this.structs = structs;
        this.functions = functions;
        this.globals = globals;
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

        foreach (g; this.globals)
        {
            ret ~= g.toString() ~ "\n";
        }

        foreach (s; this.structs)
        {
            ret ~= s.toString() ~ "\n";
        }

        foreach (func; this.functions)
        {
            ret ~= func.toString() ~ "\n";
        }

        return ret;
    }

    Function[] justFunctions()
    {
        Function[] ret;

        foreach (func; this.functions)
        {
            if (cast(Method)func is null)
            {
                ret ~= func;
            }
        }

        return ret;
    }

    Method[] justMethods()
    {
        Method[] ret;

        foreach (func; this.functions)
        {
            auto method = cast(Method)func;
            if (method !is null)
            {
                ret ~= method;
            }
        }

        return ret;
    }

    bool functionExists(string name)
    {
        foreach (func; this.justFunctions())
        {
            if (func.signature.name == name)
            {
                return true;
            }
        }

        return false;
    }

    Function findFunction(string name)
    {
        foreach (func; this.justFunctions())
        {
            if (func.signature.name == name)
            {
                return func;
            }
        }

        throw new Exception(format("Function %s not found", name));
    }

    MethodSignature findMethod(MethodCall call)
    {
        // Search the current module
        foreach (method; this.justMethods())
        {
            if (method.methodSignature.matchMethodCall(call))
            {
                return method.methodSignature;
            }
        }

        // TODO: Search imports
        throw new Exception(format("Method %s not found", call));
    }

    FunctionSignature findFunction(Call call)
    {
        if (call.functionName == "syscall")
        {
            return new FunctionSignature(new PrimitiveType(Primitive.U64),
                    "syscall", [], false);
        }

        if (call.functionName == "variadic")
        {
            return new FunctionSignature(
                    new PrimitiveType(Primitive.U64),
                    "variadic",
                    [new TypeSignature(new PrimitiveType(Primitive.U64),
                                       "index")],
                    false);
        }

        // Search the current module
        if (call.moduleName is null)
        {
            foreach (func; this.justFunctions())
            {
                if (func.signature.name == call.functionName)
                {
                    return func.signature;
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

            throw new Exception(format("Function %s not found",
                    call.functionName));
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
