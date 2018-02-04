import std.stdio;
import std.format;
import std.conv;
import std.algorithm;
import std.array;
import std.string;

import terminal;
import lexer;

int nextId = 0;

enum Primitive
{
    U64,
    U8,
    Bool,
    Auto,
}

class NotFoundException : Exception
{
    this(string msg)
    {
        super(msg);
    }
}

interface HasToken
{
    Token getToken();
}

mixin template HasTokenDefault()
{
    Token token;

    override Token getToken()
    {
        return token;
    }
}

string indent(string s)
{
    auto lines = map!(l => "    " ~ l)(s.splitLines());

    return join(lines, "\n");
}

abstract class Type : HasToken
{
    mixin HasTokenDefault;

    // The number of pointers this represents:
    //   u64   a -> 0
    //   u64*  a -> 1
    //   u64** a -> 2
    int pointerDepth;
    Node elements;

    this(int pointerDepth=0, Node elements=null, Token token=null)
    {
        this.token = token;
        this.pointerDepth = pointerDepth;
        this.elements = elements;
    }

    abstract string baseTypeToString();

    override string toString()
    {
        auto ret = this.baseTypeToString();

        if (this.elements is null)
        {
            for (int i = pointerDepth; i > 0; i--)
            {
                ret ~= "*";
            }
        }
        else
        {
            ret ~= format("[%s]", this.elements);
        }

        // TODO: crappy cast hack
        if (cast(IncompleteType)this is null)
        {
            ret = colorGreen(ret);
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
    string moduleName;
    string name;
    Type[] typeParameters;

    this(string name, Type[] typeParameters, int pointerDepth=0,
            Node elements=null, string moduleName=null, Token token=null)
    {
        super(pointerDepth=pointerDepth, elements=elements, token=token);
        this.name = name;
        this.typeParameters = typeParameters;
        this.moduleName = moduleName;
    }

    override string baseTypeToString()
    {
        return this.name;
    }

    override string toString()
    {
        string typeParams = "";

        foreach (t; this.typeParameters)
        {
            if (typeParams != "")
            {
                typeParams ~= ", ";
            }

            typeParams ~= t.toString();
        }

        if (typeParams != "")
        {
            typeParams = "<" ~ typeParams ~ ">";
        }

        string mod = "";
        if (this.moduleName !is null)
        {
            mod = moduleName ~ "::";
        }

        return colorYellow(
                format("%s%s%s", mod, super.toString(), typeParams));
    }

    override Type clone()
    {
        return new IncompleteType(this.name, this.typeParameters,
                pointerDepth=this.pointerDepth, elements=this.elements,
                moduleName=this.moduleName, token=this.token);
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

    this(Struct struct_, int pointerDepth=0, Node elements=null,
            Token token=null)
    {
        super(pointerDepth=pointerDepth, elements=elements, token=token);
        this.struct_ = struct_;
    }

    override string baseTypeToString()
    {
        return to!string(this.struct_.name);
    }

    override Type clone()
    {
        return new StructType(this.struct_, pointerDepth=this.pointerDepth,
                elements=this.elements, token=this.token);
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
        if (this.pointerDepth > 0)
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
    this(Token token = null)
    {
        super(pointerDepth=0, elements=null, token=token);
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

    this(Primitive primitive, int pointerDepth=0, Node elements=null,
            Token token=null)
    {
        super(pointerDepth=pointerDepth, elements=elements, token=token);
        this.primitive = primitive;
    }

    override string baseTypeToString()
    {
        return primitiveToString(this.primitive);
    }

    override Type clone()
    {
        return new PrimitiveType(this.primitive,
                pointerDepth=this.pointerDepth, elements=this.elements,
                token=this.token);
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
               this.pointerDepth == otherPrimitive.pointerDepth &&
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
               this.primitive == Primitive.U64 && other.pointerDepth > 0 ||
               this.primitive == Primitive.Auto ||
               otherPrimitive.primitive == Primitive.Auto ||
               this.pointerDepth > 0 &&
                    otherPrimitive.primitive == Primitive.U64;
    }

    override int baseTypeSize()
    {
        if (this.pointerDepth > 0 && this.elements is null)
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

string primitiveToString(Primitive p)
{
    switch (p)
    {
        case Primitive.U64:
            return "u64";
        case Primitive.Bool:
            return "bool";
        case Primitive.U8:
            return "u8";
        case Primitive.Auto:
            return "auto";
        default:
            throw new Exception(format("Unrecognized primitive: %s", p));
    }
}

interface InsideFunction
{
    FunctionSignature containingFunction();
}

abstract class Node : InsideFunction
{
    Token token;
    Type type;
    InsideFunction parent;

    // TODO: this sucks, shouldn't need this.
    string         tag;

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

    Node clone()
    {
        throw new Exception("No clone() implementation for this Node");
    }

    // Walk the tree replacing template type parameters with the given
    // actual types.
    void templateReplace(TypeParameter[] params, Type[] types)
    {
        foreach (c; this.childNodes())
        {
            c.templateReplace(params, types);
        }
    }
}

enum OperatorType
{
    Plus,
    Minus,
    Divide,
    Multiply,
    Modulo,
    Equality,
    Inequality,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    LogicalAnd,
    LogicalOr,
    DotAccessor,
    LeftShift,
    RightShift,
    BitwiseAnd,
    Reference,
    Dereference,
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
        case OperatorType.Multiply:
        case OperatorType.Divide:
        case OperatorType.Modulo:
        case OperatorType.LeftShift:
        case OperatorType.RightShift:
        case OperatorType.BitwiseAnd:
            return OperatorClass.Math;
        case OperatorType.Equality:
        case OperatorType.Inequality:
        case OperatorType.GreaterThan:
        case OperatorType.GreaterThanOrEqual:
        case OperatorType.LessThan:
        case OperatorType.LessThanOrEqual:
            return OperatorClass.Relational;
        case OperatorType.LogicalAnd:
        case OperatorType.LogicalOr:
            return OperatorClass.Logical;
        default:
            throw new Exception(format("Unrecognized OperatorType: %s", t));
    }
}

string operatorTypeToString(OperatorType t)
{
    switch (t)
    {
        case OperatorType.Plus:
            return "+";
        case OperatorType.Minus:
            return "-";
        case OperatorType.Divide:
            return "/";
        case OperatorType.Multiply:
            return "*";
        case OperatorType.Modulo:
            return "%";
        case OperatorType.Equality:
            return "==";
        case OperatorType.Inequality:
            return "!=";
        case OperatorType.GreaterThan:
            return ">";
        case OperatorType.GreaterThanOrEqual:
            return ">=";
        case OperatorType.LessThan:
            return "<";
        case OperatorType.LessThanOrEqual:
            return "<=";
        case OperatorType.LogicalAnd:
            return "&&";
        case OperatorType.LogicalOr:
            return "||";
        case OperatorType.DotAccessor:
            return ".";
        case OperatorType.LeftShift:
            return "<<";
        case OperatorType.RightShift:
            return ">>";
        case OperatorType.BitwiseAnd:
            return "&";
        case OperatorType.Dereference:
            return "*";
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

    override Node clone()
    {
        return new Operator(this.left.clone(), this.operatorType,
                this.right.clone());
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
        return format("(%s %s %s)", left, operatorTypeToString(operatorType),
                right);
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

    override Node clone()
    {
        return new Binding(this.local !is null ? this.local.clone() : null,
                this.name);
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
        super(new PrimitiveType(Primitive.U64));
        this.value = value;
    }

    override Node clone()
    {
        return new U64Literal(this.value);
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
        super(new PrimitiveType(Primitive.U8));
        this.value = value;
    }

    override Node clone()
    {
        return new U8Literal(this.value);
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
        super(new PrimitiveType(Primitive.Bool));
        this.value = value;
    }

    override Node clone()
    {
        return new BoolLiteral(this.value);
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
        auto type = new PrimitiveType(Primitive.U8);
        type.pointerDepth = 1;
        super(type);
        this.value = value;
    }

    override Node clone()
    {
        return new StringLiteral(this.value);
    }

    override string toString()
    {
        return format("\"%s\"", this.value.replace("\n", "\\n"));
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

    override Node clone()
    {
        return new SizeOf(this.argument.clone());
    }

    override string toString()
    {
        return format("sizeof(%s)", this.argument);
    }

    override void templateReplace(TypeParameter[] params, Type[] types)
    {
        super.templateReplace(params, types);

        this.argument = replaceTypeParameter(this.argument, params, types);
    }
}

class Call : Node
{
    // TODO: doesn't apply to MethodCall
    string moduleName;
    string functionName;
    Node[] parameters;
    FunctionSignature targetSignature;
    Type[] typeParameters;

    this(string moduleName, string functionName, Type[] typeParameters,
            Node[] parameters)
    {
        this.moduleName = moduleName;
        this.functionName = functionName;
        this.typeParameters = typeParameters;
        this.parameters = parameters;

        foreach (param; this.parameters)
        {
            param.parent = this;
        }
    }

    override Node clone()
    {
        Node[] params;
        foreach (p; this.parameters)
        {
            params ~= p.clone();
        }

        Type[] typeParams;
        foreach (t; this.typeParameters)
        {
            typeParams ~= t.clone();
        }

        return new Call(moduleName, functionName, typeParams, params);
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
        auto moduleName = "";
        if (this.moduleName !is null)
        {
            moduleName = this.moduleName ~ "::";
        }

        auto typeParams = "";
        if (this.typeParameters.length > 0)
        {
            foreach (typeParam; this.typeParameters)
            {
                if (typeParams != "")
                {
                    typeParams ~= ", ";
                }

                typeParams ~= typeParam.toString();
            }

            typeParams = "<" ~ typeParams ~ ">";
        }

        string[] params;
        foreach (p; parameters)
        {
            params ~= p.toString();
        }

        return format("%s%s%s(%s)", moduleName, this.functionName,
                typeParams, join(params, ", "));
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
        super(null, functionName, [], parameters);

        this.container = container;

        this.container.parent = this;
    }

    override Node clone()
    {
        Node[] params;
        foreach (p; this.parameters)
        {
            params ~= p.clone();
        }

        return new MethodCall(this.container.clone(), this.functionName,
                params);
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
        if (target !is null)
        {
            this.target = target;
            this.target.parent = this;
        }

        this.type = newType;
    }

    override Node clone()
    {
        return new Cast(this.type.clone(), this.target.clone());
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

    override Node clone()
    {
        return new Reference(this.source.clone());
    }

    override void retype()
    {
        if (this.source.type is null)
        {
            return;
        }

        this.type = this.source.type.clone();

        // Array decays into pointer
        if (this.type.elements !is null)
        {
            this.type.elements = null;
        }
        // Pointer becomes pointer to pointer
        else
        {
            this.type.pointerDepth++;
        }

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

    override Node clone()
    {
        return new Dereference(this.source.clone());
    }

    override void retype()
    {
        if (this.source.type is null)
        {
            return;
        }

        this.type = this.source.type.clone();
        this.type.pointerDepth--;

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

    override Node clone()
    {
        auto ret = new DotAccessor(this.container.clone(), this.memberName);
        if (this.member !is null)
        {
            ret.member = this.member.clone();
        }
        return ret;
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

    TypeSignature clone()
    {
        return new TypeSignature(this.type.clone(), this.name);
    }
}

class Struct
{
    string name;
    TypeSignature[] members;
    Method[] methods;

    // Whether this struct (and all of its methods) is available for import
    // by other modules.
    bool exported;

    this(string name, TypeSignature[] members, bool exported=false)
    {
        this.name = name;
        this.members = members;
        this.exported = exported;
    }

    override string toString()
    {
        string ret = exported ? "export " : "";
        ret ~= "struct " ~ this.name ~ "\n";
        ret ~= "{\n";

        foreach (member; this.members)
        {
            ret ~= indent(member.toString()) ~ ";\n";
        }

        ret ~= "}\n";

        foreach (method; this.methods)
        {
            ret ~= method.toString();
        }

        return ret;
    }

    // TODO: pretty copypasta, unify methods with functions
    MethodSignature findMethod(MethodCall call)
    {
        MethodSignature[] candidates;

        foreach (method; this.methods)
        {
            if (method.methodSignature.name == call.functionName)
            {
                candidates ~= method.methodSignature;
            }
        }

        foreach (c; candidates)
        {
            if (matchFunctionToCall(c, call))
            {
                return c;
            }
        }

        throw new Exception(format("Method %s not found", call));
    }
}

class StructRendering
{
    StructTemplate structTemplate;
    Struct rendering;
    Type[] typeParameters;

    this(StructTemplate structTemplate, Struct rendering,
            Type[] typeParameters)
    {
        this.structTemplate = structTemplate;
        this.rendering = rendering;
        this.typeParameters = typeParameters;
    }

    Method renderMethod(Method original)
    {
        FunctionSignature signature;
        Block block;
        renderAsTemplate(this.structTemplate.typeParameters,
                this.typeParameters, original.signature, original.block,
                signature, block);
        auto methodSig = cast(MethodSignature)signature;
        if (methodSig is null)
        {
            throw new Exception("Rendered signature is wrong type");
        }
        methodSig.containerType = new StructType(this.rendering);
        return new Method(methodSig, block);
    }
}

class StructTemplate : Struct
{
    TypeParameter[] typeParameters;
    StructRendering[] renderings;

    this(string name, TypeParameter[] typeParameters, TypeSignature[] members,
         bool exported=false)
    {
        super(name, members, exported=exported);
        this.typeParameters = typeParameters;
    }

    override string toString()
    {
        return "template " ~ super.toString();
    }

    StructRendering render(Type[] types)
    {
        // Mangle the name
        auto name = "template_" ~ this.name;
        foreach (t; types)
        {
            name ~= "_" ~ templateTypeName(t);
        }

        // Replace type parameter names in members
        TypeSignature[] members;
        foreach (member; this.members)
        {
            auto cloned = member.clone();
            cloned.type = replaceTypeParameter(cloned.type,
                    this.typeParameters, types);
            members ~= cloned;
        }

        auto ret = new StructRendering(this, new Struct(name, members), types);
        this.renderings ~= ret;
        return ret;
    }
}

abstract class StatementBase : InsideFunction
{
    Token token;
    InsideFunction parent;
    int id;

    this(Token token)
    {
        this.token = token;
        this.id = nextId++;
    }

    StatementBase clone();

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

    While containingWhile()
    {
        auto parentWhile = cast(While)parent;
        if (parentWhile !is null)
        {
            return parentWhile;
        }

        auto parentStatementBase = cast(StatementBase)parent;
        if (parentStatementBase !is null)
        {
            return parentStatementBase.containingWhile();
        }

        return null;
    }

    // Walk the tree replacing template type parameters with the given
    // actual types.
    void templateReplace(TypeParameter[] params, Type[] types)
    {
        foreach (c; this.childStatements())
        {
            c.templateReplace(params, types);
        }

        foreach (c; this.childNodes())
        {
            c.templateReplace(params, types);
        }
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
        this.index.parent = this;

        this.retype();
    }

    override Node clone()
    {
        return new Indexer(this.source.clone(), this.index.clone());
    }

    override void retype()
    {
        if (this.source.type is null)
        {
            return;
        }

        this.type = this.source.type.clone();
        this.type.pointerDepth--;
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

    this(Token token, Node expression)
    {
        super(token);

        this.expression = expression;

        if (this.expression !is null)
        {
            this.expression.parent = this;
        }
    }

    override StatementBase clone()
    {
        return new Statement(this.token, this.expression.clone());
    }

    override string toString()
    {
        return expression.toString();
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

    this(Token token, TypeSignature signature, Node value)
    {
        super(token);

        this.signature = signature;
        this.value = value;

        if (this.value !is null)
        {
            this.value.parent = this;
        }
    }

    override StatementBase clone()
    {
        return new LocalDeclaration(this.token, this.signature.clone(),
                this.value is null ? null : this.value.clone());
    }

    override string toString()
    {
        const string assignment =
                this.value !is null ? " = " ~ this.value.toString() : "";

        return format("%s%s;", signature, assignment);
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

    // Walk the tree replacing template type parameters with the given
    // actual types.
    override void templateReplace(TypeParameter[] params, Type[] types)
    {
        super.templateReplace(params, types);

        this.signature.type = replaceTypeParameter(this.signature.type, params,
                types);
    }
}

class Assignment : StatementBase
{
    Node lvalue;
    Node value;

    this(Token token, Node lvalue, Node value)
    {
        super(token);

        this.lvalue = lvalue;
        this.value = value;

        this.lvalue.parent = this;
        this.value.parent = this;
    }

    override StatementBase clone()
    {
        return new Assignment(this.token, this.lvalue.clone(),
                this.value.clone());
    }

    override string toString()
    {
        return format("%s = %s;", this.lvalue, this.value);
    }

    override Node[] childNodes()
    {
        return [this.lvalue, this.value];
    }
}

class Break : StatementBase
{
    this(Token token)
    {
        super(token);
    }

    override StatementBase clone()
    {
        return new Break(this.token);
    }

    override string toString()
    {
        return "break";
    }
}

class Continue : StatementBase
{
    this(Token token)
    {
        super(token);
    }

    override StatementBase clone()
    {
        return new Continue(this.token);
    }

    override string toString()
    {
        return "continue";
    }
}

class Defer : StatementBase
{
    StatementBase statement;

    this(Token token, StatementBase statement)
    {
        super(token);

        this.statement = statement;

        this.statement.parent = this;
    }

    override StatementBase clone()
    {
        return new Defer(this.token, this.statement.clone());
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

    this(Token token, Node conditional, StatementBase block)
    {
        super(token);

        this.conditional = conditional;
        this.block = block;

        this.conditional.parent = this;
        this.block.parent = this;
    }

    override StatementBase clone()
    {
        return new ConditionalBlock(this.token, this.conditional.clone(),
                this.block.clone());
    }

    override string toString()
    {
        return format("%s\n%s", this.conditional, this.block);
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
    this(Token token, Node conditional, StatementBase block)
    {
        super(token, conditional, block);
    }

    override StatementBase clone()
    {
        return new While(this.token, this.conditional.clone(),
                this.block.clone());
    }

    override string toString()
    {
        return format("while %s", super.toString());
    }

    string startLabel()
    {
        return format("while_start_%d", this.id);
    }

    string endLabel()
    {
        return format("while_end_%d", this.id);
    }
}

class If : StatementBase
{
    ConditionalBlock ifBlock;
    ConditionalBlock[] elseIfBlocks;
    StatementBase elseBlock;

    this(Token token, ConditionalBlock ifBlock,
            ConditionalBlock[] elseIfBlocks, StatementBase elseBlock)
    {
        super(token);

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

    override StatementBase clone()
    {
        ConditionalBlock ifBlock = cast(ConditionalBlock)this.ifBlock.clone();
        if (ifBlock is null)
        {
            throw new Exception("Clone failed");
        }

        ConditionalBlock[] elseIfs;
        foreach (e; this.elseIfBlocks)
        {
            ConditionalBlock cloned = cast(ConditionalBlock)e.clone();
            if (cloned is null)
            {
                throw new Exception("Clone failed");
            }
            elseIfs ~= cloned;
        }

        auto elseBlock = this.elseBlock !is null ? this.elseBlock.clone() :
                this.elseBlock;

        return new If(this.token, ifBlock, elseIfs, elseBlock);
    }

    override string toString()
    {
        auto ret = format("if %s\n%s", this.ifBlock.conditional,
                          this.ifBlock.block);

        foreach (elseIf; this.elseIfBlocks)
        {
            ret ~= format("else if %s\n%s", elseIf.conditional, elseIf.block);
        }

        if (this.elseBlock !is null)
        {
            ret ~= format("else\n%s", this.elseBlock);
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

    this(Token token, Node expression)
    {
        super(token);

        this.expression = expression;

        if (this.expression !is null)
        {
            this.expression.parent = this;
        }
    }

    override StatementBase clone()
    {
        return new Return(this.token, this.expression.clone());
    }

    override string toString()
    {
        return format("return %s;", this.expression);
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

    override StatementBase clone()
    {
        StatementBase[] st;
        foreach (s; this.statements)
        {
            st ~= s.clone();
        }

        return new Block(st);
    }

    override string toString()
    {
        auto ret = "{\n";

        foreach (statement; this.statements)
        {
            ret ~= indent(statement.toString()) ~ "\n";
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

class TypeParameter
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

class FunctionSignature
{
    Module mod;
    Type returnType;
    string name;
    TypeSignature[] parameters;
    bool variadic;

    // Whether the function is available for import by other modules
    bool exported;

    // For generating return variables and other things that can't clash within
    // a function.
    int nextGeneratedIndex;

    // Whether this function is overloaded elsewhere
    bool overloaded;

    this(Type returnType, string name, TypeSignature[] parameters,
         bool variadic, bool exported=false)
    {
        this.returnType = returnType;
        this.name = name;
        this.parameters = parameters;
        this.variadic = variadic;
        this.exported = exported;
        this.nextGeneratedIndex = 0;
        this.overloaded = false;
    }

    FunctionSignature clone()
    {
        TypeSignature[] params;
        foreach (p; this.parameters)
        {
            params ~= p.clone();
        }

        auto ret = new FunctionSignature(this.returnType.clone(), this.name,
                params, this.variadic);

        ret.mod = this.mod;

        return ret;
    }

    string fullName()
    {
        return this.name;
    }

    override string toString()
    {
        string ret = returnType.toString() ~ " " ~ this.fullName();

        // Parameters
        string[] params;
        foreach (p; parameters)
        {
            params ~= p.toString();
        }

        if (variadic)
        {
            params ~= "...";
        }

        return ret ~ "(" ~ join(params, ", ") ~ ")";
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

    override FunctionSignature clone()
    {
        TypeSignature[] params;
        foreach (p; this.parameters)
        {
            params ~= p.clone();
        }

        auto ret = new MethodSignature(this.returnType.clone(),
                this.containerType.clone(), this.name, params, this.variadic);

        ret.mod = this.mod;

        return ret;
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

class FunctionRendering
{
    FunctionTemplate functionTemplate;
    Function rendering;
    Type[] typeParameters;

    this(FunctionTemplate functionTemplate, Function rendering,
            Type[] typeParameters)
    {
        this.functionTemplate = functionTemplate;
        this.rendering = rendering;
        this.typeParameters = typeParameters;
    }
}

Type replaceTypeParameter(Type type, TypeParameter[] params,
        Type[] replacements)
{
    if (params.length != replacements.length)
    {
        throw new Exception("Type parameter count mismatch");
    }

    auto incomplete = cast(IncompleteType)type;
    if (incomplete is null)
    {
        return type;
    }

    if (incomplete.typeParameters.length > 0)
    {
        throw new Exception("Not implemented");
    }

    for (int i = 0; i < params.length; i++)
    {
        if (incomplete.name == params[i].name)
        {
            auto ret = replacements[i].clone();
            ret.pointerDepth += type.pointerDepth;
            return ret;
        }
    }

    return type;
}

// Get the name of a type for use in function name mangling for templates
string templateTypeName(Type t)
{
    string ret = t.baseTypeToString();

    for (int i = 0; i < t.pointerDepth; i++)
    {
        ret ~= "p";
    }

    return ret;
}

string templateName(string name, Type[] types)
{
    auto ret = "template_" ~ name;

    foreach (t; types)
    {
        ret ~= "_" ~ templateTypeName(t);
    }

    return ret;
}

// TODO: make this part of FunctionTemplate or something? The problem is
// method templates need this functionality but there isn't really a
// MethodTemplate object, just a StuctTemplate.
void renderAsTemplate(TypeParameter[] typeParams, Type[] types,
        FunctionSignature originalSignature, StatementBase originalBlock,
        out FunctionSignature signature, out Block block)
{
    signature = originalSignature.clone();
    signature.name = templateName(signature.name, types);
    signature.returnType = replaceTypeParameter(signature.returnType,
            typeParams, types);

    foreach (p; signature.parameters)
    {
        p.type = replaceTypeParameter(p.type, typeParams, types);
    }

    block = cast(Block)originalBlock.clone();
    if (block is null)
    {
        throw new Exception("Clone failure");
    }

    block.templateReplace(typeParams, types);
}

class FunctionTemplate : Function
{
    TypeParameter[] typeParameters;
    FunctionRendering[] renderings;

    this(FunctionSignature signature, Block block,
            TypeParameter[] typeParameters)
    {
        super(signature, block);

        this.typeParameters = typeParameters;
        this.renderings = [];
    }

    FunctionRendering render(Type[] types)
    {
        FunctionSignature signature;
        Block block;
        renderAsTemplate(this.typeParameters, types, this.signature,
                this.block, signature, block);
        return new FunctionRendering(this, new Function(signature, block),
                types);
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
    FunctionTemplate[] functionTemplates;
    Struct[] structs;
    StructTemplate[] structTemplates;

    // Unqualified imports do not require a module name and scope operator to
    // references their exports
    bool unqualified;

    this(string filename, string name, FunctionSignature[] functions,
            FunctionTemplate[] functionTemplates, Struct[] structs,
            StructTemplate[] structTemplates, bool unqualified=false)
    {
        this.filename = filename;
        this.name = name;
        this.functions = functions;
        this.functionTemplates = functionTemplates;
        this.structs = structs;
        this.structTemplates = structTemplates;
        this.unqualified = unqualified;
    }

    override string toString()
    {
        return format("import %s;", this.name);
    }

    FunctionSignature[] findFunctions(string name)
    {
        FunctionSignature[] ret;

        foreach (func; this.functions)
        {
            if (func.name == name)
            {
                ret ~= func;
            }
        }

        return ret;
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

bool matchFunctionToCall(FunctionSignature func, Call call)
{
    if (func.name != call.functionName)
    {
        return false;
    }

    bool isMethod = cast(MethodSignature)func !is null;

    // Validate arguments
    TypeSignature[] funcParams;
    for (auto i = 0; i < func.parameters.length; i++)
    {
        // Skip the first parameter if this is a method call and this has been
        // injected
        if (isMethod && i == 0 && func.parameters[i].name == "this")
        {
            continue;
        }

        funcParams ~= func.parameters[i];
    }

    Node[] callArgs;
    foreach (p; call.parameters)
    {
        callArgs ~= p;

        if (func.variadic && callArgs.length >= func.parameters.length)
        {
            break;
        }
    }

    if (funcParams.length != callArgs.length)
    {
        return false;
    }

    for (auto i = 0; i < callArgs.length; i++)
    {
        if (!funcParams[i].type.compare(callArgs[i].type))
        {
            return false;
        }
    }

    return true;
}

class Module
{
    string name;
    Import[] imports;
    Struct[] structs;
    StructTemplate[] structTemplates;
    Function[] functions;
    Global[] globals;
    FunctionSignature[] externs;

    this(string name, Import[] imports, Struct[] structs, Function[] functions,
         Global[] globals, FunctionSignature[] externs,
         StructTemplate[] structTemplates)
    {
        this.name = name;
        this.imports = imports;
        this.structs = structs;
        this.functions = functions;
        this.globals = globals;
        this.externs = externs;
        this.structTemplates = structTemplates;
    }

    override string toString()
    {
        auto ret = "";

        foreach (imp; this.imports)
        {
            ret ~= imp.toString() ~ "\n";

            foreach (st; imp.structTemplates)
            {
                foreach (r; st.renderings)
                {
                    ret ~= r.rendering.toString() ~ "\n";
                }
            }
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

        foreach (s; this.structTemplates)
        {
            ret ~= s.toString() ~ "\n";

            foreach (r; s.renderings)
            {
                ret ~= r.rendering.toString() ~ "\n";
            }
        }

        foreach (func; this.functions)
        {
            ret ~= func.toString() ~ "\n";
        }

        return ret;
    }

    Function[] functionsAndMethods()
    {
        Function[] ret;

        foreach (func; this.functions)
        {
            if (cast(FunctionTemplate)func is null)
            {
                ret ~= func;
            }
        }

        foreach (st; this.structs)
        {
            ret ~= st.methods;
        }

        return ret;
    }

    Function[] justFunctions()
    {
        Function[] ret;

        foreach (func; this.functions)
        {
            if (cast(Method)func is null && cast(FunctionTemplate)func is null)
            {
                ret ~= func;
            }
        }

        return ret;
    }

    FunctionTemplate[] justFunctionTemplates()
    {
        FunctionTemplate[] ret;

        foreach (func; this.functions)
        {
            auto ft = cast(FunctionTemplate)func;
            if (ft !is null)
            {
                ret ~= ft;
            }
        }

        return ret;
    }

    FunctionTemplate[] unqualifiedFunctionTemplates()
    {
        FunctionTemplate[] ret = this.justFunctionTemplates();

        foreach (imp; this.imports)
        {
            if (imp.unqualified)
            {
                ret ~= imp.functionTemplates;
            }
        }

        return ret;
    }

    FunctionSignature[] findFunctions(string moduleName, string functionName)
    {
        // Search imported modules
        if (moduleName !is null)
        {
            auto imp = this.findImport(moduleName);
            return imp.findFunctions(functionName);
        }

        FunctionSignature[] ret;

        // Search functions
        foreach (func; this.justFunctions())
        {
            if (func.signature.name == functionName)
            {
                ret ~= func.signature;
            }
        }

        // Search for externs defined in the current module
        foreach (ext; this.externs)
        {
            if (ext.name == functionName)
            {
                ret ~= ext;
            }
        }

        // Search unqualified imports
        foreach (imp; this.imports)
        {
            foreach (f; imp.functions)
            {
                if (f.name == functionName)
                {
                    ret ~= f;
                }
            }
        }

        return ret;
    }

    FunctionSignature findFunction(Call call)
    {
        // Built-in: syscall
        if (call.functionName == "syscall")
        {
            return new FunctionSignature(new PrimitiveType(Primitive.U64),
                    "syscall", [], false);
        }

        // Built-in: variadic
        if (call.functionName == "variadic")
        {
            return new FunctionSignature(
                    new PrimitiveType(Primitive.U64),
                    "variadic",
                    [new TypeSignature(new PrimitiveType(Primitive.U64),
                                       "index")],
                    false);
        }

        auto candidates = this.findFunctions(call.moduleName,
                call.functionName);

        foreach (c; candidates)
        {
            if (matchFunctionToCall(c, call))
            {
                return c;
            }
        }

        // TODO: this can be misleading when the problem is a type mismatch
        throw new Exception(format("Function %s not found",
                call.functionName));
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

        throw new NotFoundException(format("Import %s not found", name));
    }

    // If the given struct is a rendering of a struct template, find that
    // rendering. Otherwise, return null.
    StructRendering findStructRendering(Struct struct_)
    {
        foreach (st; this.structTemplates)
        {
            foreach (r; st.renderings)
            {
                if (r.rendering is struct_)
                {
                    return r;
                }
            }
        }

        // Look in imports
        foreach (imp; this.imports)
        {
            foreach (st; imp.structTemplates)
            {
                foreach (r; st.renderings)
                {
                    if (r.rendering is struct_)
                    {
                        return r;
                    }
                }
            }
        }

        return null;
    }

    StructTemplate findStructTemplate(string moduleName, string structName)
    {
        StructTemplate[] search = this.structTemplates;

        // Find qualified import
        if (moduleName !is null)
        {
            search = this.findImport(moduleName).structTemplates;
        }
        // Find unqualified imports
        else
        {
            foreach (imp; this.imports)
            {
                if (imp.unqualified)
                {
                    search ~= imp.structTemplates;
                }
            }
        }

        foreach (st; search)
        {
            if (st.name == structName)
            {
                return st;
            }
        }

        throw new NotFoundException("Struct not found");
    }
}
