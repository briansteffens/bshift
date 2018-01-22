import std.stdio;
import std.format;
import std.conv;
import std.array;
import std.file;
import std.algorithm;

import globals;
import terminal;
import ast;
import lexer;
import validator;

OperatorType operatorType(string input)
{
    switch (input)
    {
        case "+":
            return OperatorType.Plus;
        case "-":
            return OperatorType.Minus;
        case "*":
            return OperatorType.Multiply;
        case "/":
            return OperatorType.Divide;
        case "%":
            return OperatorType.Modulo;
        case "==":
            return OperatorType.Equality;
        case "!=":
            return OperatorType.Inequality;
        case ">":
            return OperatorType.GreaterThan;
        case ">=":
            return OperatorType.GreaterThanOrEqual;
        case "<":
            return OperatorType.LessThan;
        case "<=":
            return OperatorType.LessThanOrEqual;
        case "&&":
            return OperatorType.LogicalAnd;
        case "||":
            return OperatorType.LogicalOr;
        case ".":
            return OperatorType.DotAccessor;
        case "<<":
            return OperatorType.LeftShift;
        case ">>":
            return OperatorType.RightShift;
        case "&":
            return OperatorType.BitwiseAnd;
        case "dereference":
            return OperatorType.Dereference;
        default:
            throw new Exception("We will re-raise this with more info later.");
    }
}

int operatorPrecedence(OperatorType t)
{
    switch (t)
    {
        case OperatorType.DotAccessor:
            return 19;
        case OperatorType.Dereference:
            return 18;
        case OperatorType.Multiply:
        case OperatorType.Divide:
        case OperatorType.Modulo:
            return 14;
        case OperatorType.Plus:
        case OperatorType.Minus:
            return 13;
        case OperatorType.LeftShift:
        case OperatorType.RightShift:
            return 12;
        case OperatorType.LessThan:
        case OperatorType.LessThanOrEqual:
        case OperatorType.GreaterThan:
        case OperatorType.GreaterThanOrEqual:
            return 11;
        case OperatorType.Equality:
        case OperatorType.Inequality:
            return 10;
        case OperatorType.BitwiseAnd:
            return 9;
        case OperatorType.LogicalAnd:
            return 6;
        case OperatorType.LogicalOr:
            return 5;
        default:
            throw new Exception("We'll re-raise this later with more info.");
    }
}

int operatorInputCount(OperatorType t)
{
    switch (t)
    {
        case OperatorType.Dereference:
            return 1;
        case OperatorType.Plus:
        case OperatorType.Minus:
        case OperatorType.Divide:
        case OperatorType.Multiply:
        case OperatorType.Modulo:
        case OperatorType.Equality:
        case OperatorType.Inequality:
        case OperatorType.GreaterThan:
        case OperatorType.GreaterThanOrEqual:
        case OperatorType.LessThan:
        case OperatorType.LessThanOrEqual:
        case OperatorType.LogicalAnd:
        case OperatorType.LogicalOr:
        case OperatorType.DotAccessor:
        case OperatorType.LeftShift:
        case OperatorType.RightShift:
        case OperatorType.BitwiseAnd:
            return 2;
        default:
            throw new Exception("We'll re-raise this later with more info.");
    }
}

// This is raised when, say, parseStruct is called on an if statement: the
// grammar doesn't match but it doesn't necessarily mean the tokens are
// invalid syntax.
class GrammarMismatch : Exception
{
    this(string efile = __FILE__, size_t eline = __LINE__)
    {
        super("The tokens could not be parsed as the given grammar rule",
                efile, eline);
    }
}

// This is raised when the parser malfunctions.
class ParserException : Exception
{
    this(string s, string efile = __FILE__, size_t eline = __LINE__)
    {
        super(s, efile, eline);
    }
}

class TokenFeed
{
    Token[] tokens;
    int index;

    this(Token[] tokens)
    {
        this.tokens = tokens;
        this.index = 0;
    }

    TokenFeed shallowCopy()
    {
        auto ret = new TokenFeed(this.tokens[]);

        ret.index = this.index;

        return ret;
    }

    @property bool eof()
    {
        return index == tokens.length;
    }

    bool next()
    {
        if (eof)
        {
            throw new ParserException("Attempt to read beyond end of tokens");
        }

        this.index++;

        return !eof;
    }

    bool inBounds(int index)
    {
        return index >= 0 && index < this.tokens.length;
    }

    bool seek(int distance)
    {
        auto changed = this.index + distance;

        if (!inBounds(changed))
        {
            return false;
        }

        this.index = changed;
        return true;
    }

    Token peek(int distance)
    {
        int target = this.index + distance;

        if (!inBounds(target))
        {
            return null;
        }

        return this.tokens[target];
    }

    @property Token current()
    {
        return this.tokens[this.index];
    }

    // If the current token matches the given criteria, return it and advance
    // the reader. Otherwise, throw GrammarMismatch.
    Token expect(TokenType type, string value = null)
    {
        auto ret = this.current;

        if (!ret.match(type, value))
        {
            throw new GrammarMismatch();
        }

        this.next();

        return ret;
    }

    Token expectWord(string value = null)
    {
        return expect(TokenType.Word, value);
    }

    Token expectSymbol(string value = null)
    {
        return expect(TokenType.Symbol, value);
    }

    // If the current token matches the criteria, return it and advance the
    // reader. Otherwise, throw SyntaxError.
    Token require(TokenType type, string value = null,
            string errorMessage = null)
    {
        if (!match(type, value))
        {
            if (errorMessage is null)
            {
                errorMessage = format("At [%s], expected a %s of '%s'.",
                        current, type, value);
            }

            throw new SyntaxError(errorMessage, current);
        }

        auto ret = current;

        this.next();

        return ret;
    }

    Token requireWord(string value = null, string errorMessage = null)
    {
        return require(TokenType.Word, value, errorMessage);
    }

    Token requireSymbol(string value = null, string errorMessage = null)
    {
        return require(TokenType.Symbol, value, errorMessage);
    }

    // Checks if the current token matches the criteria
    bool match(TokenType type, string value = null)
    {
        return current.match(type, value);
    }

    bool matchWord(string value = null)
    {
        return match(TokenType.Word, value);
    }

    bool matchSymbol(string value = null)
    {
        return match(TokenType.Symbol, value);
    }

    // If the current token matches the criteria, skips to the next token
    bool nextIf(TokenType type, string value = null)
    {
        try
        {
            expect(type, value);
            return true;
        }
        catch (GrammarMismatch)
        {
            return false;
        }
    }

    bool nextIfWord(string value = null)
    {
        return nextIf(TokenType.Word, value);
    }

    bool nextIfSymbol(string value = null)
    {
        return nextIf(TokenType.Symbol, value);
    }

    override string toString()
    {
        int stop = this.index + 5;
        if (stop >= this.tokens.length)
        {
            stop = cast(int)this.tokens.length - 1;
        }

        string ret = "";

        for (uint i = index; i <= stop; i++)
        {
            if (i > index)
            {
                ret ~= " ";
            }

            ret ~= this.tokens[i].toString();
        }

        return ret;
    }
}

// Try to parse something. If successful, the parsed AST node is returned and
// the TokenFeed is advanced. If the parser throws GrammarMismatch, _default is
// returned and the TokenFeed is unchanged.
T tryParse(T)(TokenFeed tokens, T function(TokenFeed tokens) parser,
        T _default = T.init)
{
    auto copy = tokens.shallowCopy();
    T ret;

    try
    {
        ret = parser(copy);
    }
    catch (GrammarMismatch)
    {
        return _default;
    }

    // Advance token reader to consume the parsed tokens
    tokens.index = copy.index;

    return ret;
}

string resolveImportFilename(Token start, string moduleName)
{
    auto candidates = [
        format("%s.bs", moduleName),
        format("lib/%s.bs", moduleName),
        format("/usr/local/lib/bshift/%s.bs", moduleName),
    ];

    foreach (candidate; candidates)
    {
        if (exists(candidate))
        {
            return candidate;
        }
    }

    throw new SyntaxError(format("Can't find a file (among %s) to match " ~
            "the imported module", candidates), start);
}

// Process import. If symbol inclusion list given, only symbols in that list
// will be imported from the module. Further, they will be imported without
// module scoping.
Import[] processImport(Token start, string moduleName, string[] symbols = [])
{
    string filename = resolveImportFilename(start, moduleName);

    // Parse the import
    auto parsed = parse(moduleName, lex(readText(filename), filename));

    bool included(string symbol)
    {
        return symbols.length == 0 || symbols.canFind(symbol);
    }

    FunctionTemplate[] functionTemplates;
    FunctionSignature[] signatures;
    foreach (func; parsed.functions)
    {
        if (!func.signature.exported || !included(func.signature.name))
        {
            continue;
        }

        // Extract entire function templates but only signatures for regular
        // functions.
        auto ft = cast(FunctionTemplate)func;
        if (ft !is null)
        {
            functionTemplates ~= ft;
        }
        else
        {
            signatures ~= func.signature;
        }
    }

    Struct[] structs;
    foreach (s; parsed.structs)
    {
        if (s.exported && included(s.name))
        {
            structs ~= s;
        }
    }

    StructTemplate[] structTemplates;
    foreach (st; parsed.structTemplates)
    {
        if (st.exported && included(st.name))
        {
            structTemplates ~= st;
        }
    }

    bool unqualified = symbols.length > 0;

    return [new Import(filename, moduleName, signatures, functionTemplates,
            structs, structTemplates, unqualified=unqualified)]
           ~ parsed.imports;
}

// Parse unqualified import: import length, reverse from cstring;
Import[] parseImportFrom(TokenFeed tokens)
{
    auto start = tokens.expectWord("import");

    string[] symbols;

    while (!tokens.matchWord("from"))
    {
        symbols ~= tokens.expectWord().value;
        tokens.nextIfSymbol(",");
    }

    tokens.expectWord("from");
    auto moduleName = tokens.expectWord().value;
    tokens.expectSymbol(";");

    return processImport(start, moduleName, symbols);
}

// Parse qualified import: import io;
Import[] parseImport(TokenFeed tokens)
{
    auto start = tokens.expectWord("import");
    auto moduleName = tokens.expectWord().value;
    tokens.expectSymbol(";");

    return processImport(start, moduleName);
}

Module parse(string name, Token[] tokenArray)
{
    auto tokens = new TokenFeed(tokenArray);

    Import[] imports;
    Function[] functions;
    FunctionSignature[] externs;
    Struct[] structs;
    StructTemplate[] structTemplates;
    Global[] globals;
    Token current = tokens.current;

    while (!tokens.eof)
    {
        current = tokens.current;

        Import[] imps = tryParse(tokens, &parseImport);

        if (imps is null)
        {
            imps = tryParse(tokens, &parseImportFrom);
        }

        if (imps !is null)
        {
            imports ~= imps;

            // TODO: have a separate parsing pass for imports to avoid this
            // Clear template renderings
            foreach (imp; imports)
            {
                foreach (ft; imp.functionTemplates)
                {
                    ft.renderings = [];
                }
                foreach (st; imp.structTemplates)
                {
                    st.renderings = [];
                }
            }

            continue;
        }

        auto ext = tryParse(tokens, &parseExtern);
        if (ext !is null)
        {
            externs ~= ext;
            continue;
        }

        auto structTemplate = tryParse(tokens, &parseStructTemplate);
        if (structTemplate !is null)
        {
            structTemplates ~= structTemplate;
            continue;
        }

        auto struct_ = tryParse(tokens, &parseStruct);
        if (struct_ !is null)
        {
            structs ~= struct_;
            continue;
        }

        auto method = tryParse(tokens, &parseMethod);
        if (method !is null)
        {
            auto incomplete =
                    cast(IncompleteType)method.methodSignature.containerType;

            if (incomplete is null)
            {
                throw new Exception("Unexpected method container type");
            }

            Struct container = null;

            // Look for a matching struct
            foreach (st; structs)
            {
                if (st.name == incomplete.name)
                {
                    container = st;
                    break;
                }
            }

            // Look for a matching struct template
            if (container is null)
            {
                foreach (st; structTemplates)
                {
                    if (st.name == incomplete.name)
                    {
                        container = st;
                        break;
                    }
                }
            }

            if (container is null)
            {
                throw new Exception("Couldn't find struct for method");
            }

            container.methods ~= method;
            method.methodSignature.containerType = new StructType(container);

            continue;
        }

        auto func = tryParse(tokens, &parseFunction);
        if (func !is null)
        {
            functions ~= func;
            continue;
        }

        auto global = tryParse(tokens, &parseGlobal);
        if (global !is null)
        {
            globals ~= global;
            continue;
        }

        throw new SyntaxError(
            "Can't parse grammar near",
            current.value, current.line, current.lineOffset);
    }

    auto ret = new Module(name, imports, structs, functions, globals,
            externs, structTemplates);

    foreach (func; ret.functions)
    {
        func.signature.mod = ret;
    }

    foreach (st; ret.structs)
    {
        foreach (met; st.methods)
        {
            met.signature.mod = ret;
        }
    }

    foreach (st; ret.structTemplates)
    {
        foreach (met; st.methods)
        {
            met.signature.mod = ret;
        }
    }

    if (verbose)
    {
        writeln("bshift unvalidated ast ----------------------------------\n");
        writeln(ret);
    }

    validate(ret);

    return ret;
}

Global parseGlobal(TokenFeed tokens)
{
    auto signature = parseTypeSignature(tokens);

    Node value = null;
    if (tokens.nextIfSymbol("="))
    {
        value = parseExpression(tokens);
    }

    tokens.expectSymbol(";");

    return new Global(signature, value);
}

TypeSignature[] parseStructMembers(TokenFeed tokens)
{
    tokens.expectSymbol("{");

    TypeSignature[] members;

    while (!tokens.nextIfSymbol("}"))
    {
        members ~= parseTypeSignature(tokens);

        tokens.expectSymbol(";");
    }

    return members;
}

Struct parseStruct(TokenFeed tokens)
{
    bool exported = tokens.nextIfWord("export");

    tokens.expectWord("struct");

    auto name = tokens.requireWord().value;

    // Parse struct members
    auto members = parseStructMembers(tokens);

    return new Struct(name, members, exported=exported);
}

StructTemplate parseStructTemplate(TokenFeed tokens)
{
    bool exported = tokens.nextIfWord("export");

    tokens.expectWord("struct");

    auto name = tokens.requireWord().value;

    // Template type parameters
    auto typeParams = parseTypeParams(tokens);

    // Parse struct members
    auto members = parseStructMembers(tokens);

    return new StructTemplate(name, typeParams, members, exported=exported);
}

FunctionSignature parseExtern(TokenFeed tokens)
{
    tokens.expectWord("extern");

    TypeParameter[] typeParams;
    auto sig = parseFunctionSignature(tokens, &typeParams);

    tokens.requireSymbol(";");

    return sig;
}

FunctionSignature parseFunctionSignature(TokenFeed tokens,
        TypeParameter[]* typeParams)
{
    // Check for 'export' keyword
    bool exported = tokens.nextIf(TokenType.Word, "export");

    // Parse the return value
    auto type = parseType(tokens);

    // Function name
    auto name = tokens.requireWord().value;

    // Template type parameters
    auto typeParamsFound = tryParse(tokens, &parseTypeParams);
    if (typeParamsFound is null)
    {
        typeParamsFound = [];
    }

    foreach (typeParam; typeParamsFound)
    {
        *typeParams ~= typeParam;
    }

    // Parameters
    bool variadic;
    auto params = parseParameterList(tokens, &variadic);

    return new FunctionSignature(type, name, params, variadic,
            exported=exported);
}

MethodSignature parseMethodSignature(TokenFeed tokens)
{
    auto returnType = parseType(tokens);

    // Container type
    auto containerName = tokens.expectWord().value;
    auto containerType = new IncompleteType(containerName, []);

    // Member operator ::
    tokens.expectSymbol("::");

    auto functionName = tokens.requireWord().value;

    bool variadic;
    auto params = parseParameterList(tokens, &variadic);

    return new MethodSignature(returnType, containerType, functionName,
                               params, variadic);
}

// Parse template type params, as in: <T, U>
TypeParameter[] parseTypeParams(TokenFeed tokens)
{
    tokens.expectSymbol("<");

    TypeParameter[] ret;

    while (!tokens.nextIfSymbol(">"))
    {
        ret ~= new TypeParameter(tokens.requireWord().value);
        tokens.nextIfSymbol(",");
    }

    if (ret.length == 0)
    {
        throw new SyntaxError("No type parameters in template",
                tokens.current);
    }

    return ret;
}

// Parse concrete template type params, as in: <u64, u8>
Type[] parseConcreteTypeParams(TokenFeed tokens)
{
    tokens.expectSymbol("<");

    Type[] ret;

    while (!tokens.nextIfSymbol(">"))
    {
        ret ~= parseType(tokens);

        tokens.nextIf(TokenType.Symbol, ",");
    }

    return ret;
}

TypeSignature[] parseParameterList(TokenFeed tokens, bool* variadic)
{
    tokens.expectSymbol("(");

    TypeSignature[] ret;
    *variadic = false;

    while (!tokens.nextIfSymbol(")"))
    {
        // Variadic
        if (tokens.nextIfSymbol("..."))
        {
            tokens.requireSymbol(")");

            *variadic = true;
            break;
        }

        ret ~= parseTypeSignature(tokens);

        tokens.nextIfSymbol(",");
    }

    return ret;
}

Function parseFunction(TokenFeed tokens)
{
    TypeParameter[] typeParams;
    auto sig = parseFunctionSignature(tokens, &typeParams);

    auto functionBody = parseBlock(tokens);

    if (typeParams.length > 0)
    {
        return new FunctionTemplate(sig, functionBody, typeParams);
    }
    else
    {
        return new Function(sig, functionBody);
    }
}

Method parseMethod(TokenFeed tokens)
{
    auto sig = parseMethodSignature(tokens);
    auto functionBody = parseBlock(tokens);

    return new Method(sig, functionBody);
}

Block parseBlock(TokenFeed tokens)
{
    tokens.expectSymbol("{");

    StatementBase[] statements;

    while (!tokens.nextIfSymbol("}"))
    {
        statements ~= parseStatementBase(tokens);
    }

    return new Block(statements);
}

StatementBase parseStatementBase(TokenFeed tokens)
{
    auto current = tokens.current;

    // Start of block
    if (current.match(TokenType.Symbol, "{"))
    {
        return parseBlock(tokens);
    }

    if (current.type == TokenType.Word)
    {
        switch (current.value)
        {
            case "return":
                return parseReturn(tokens);
            case "if":
                return parseIf(tokens);
            case "while":
                return parseWhile(tokens);
            case "defer":
                return parseDefer(tokens);
            case "break":
                return parseBreak(tokens);
            case "continue":
                return parseContinue(tokens);
            default:
                auto localDeclaration = tryParse(tokens,
                        &parseLocalDeclaration);
                if (localDeclaration !is null)
                {
                    return localDeclaration;
                }
        }
    }

    auto assignment = tryParse(tokens, &parseAssignment);
    if (assignment !is null)
    {
        return assignment;
    }

    auto statement = tryParse(tokens, &parseStatement);
    if (statement !is null)
    {
        return statement;
    }

    throw new SyntaxError("Can't parse grammar", tokens.current);
}

// Try to parse a local declaration (like "u64 x = 3;") and return null if it
// can't be done.
LocalDeclaration parseLocalDeclaration(TokenFeed tokens)
{
    auto start = tokens.current;

    // Try to parse the lvalue
    auto typeSignature = parseTypeSignature(tokens);

    Node expression = null;

    // Assignment
    if (tokens.nextIfSymbol("="))
    {
        expression = parseExpression(tokens);
    }
    // Constructor
    else if (tokens.matchSymbol("("))
    {
        // Kinda weird - rewind so the expression parser sees the local name.
        // So "point p(3, 7)" gets parsed as the call "p(3, 7)". Then we can
        // replace "p" with the real constructor to be called. Just hacking
        // around the expression parser which is getting pretty due for a
        // refactor. TODO
        tokens.index -= 1;
        auto parsed = parseExpression(tokens);
        auto call = cast(Call)parsed;
        if (call is null)
        {
            throw new SyntaxError("Bad constructor call", tokens.current);
        }

        auto container = new Binding(typeSignature, call.functionName);
        expression = new MethodCall(container, "construct", call.parameters);
    }

    tokens.requireSymbol(";");

    return new LocalDeclaration(start.line, typeSignature, expression);
}

Type parseType(TokenFeed tokens)
{
    // Read either the module or type name
    string typeName = tokens.expectWord().value;
    string moduleName = null;

    // Detect scope operator, which means we have a module and a type name
    if (tokens.nextIfSymbol("::"))
    {
        moduleName = typeName;
        typeName = tokens.expectWord().value;
    }

    // Read type parameters
    auto typeParams = tryParse(tokens, &parseConcreteTypeParams, []);

    // Read pointer symbols
    int pointerDepth = 0;
    while (tokens.nextIfSymbol("*"))
    {
        pointerDepth++;
    }

    // Read array element count
    Node elements = null;
    if (tokens.nextIf(TokenType.Symbol, "["))
    {
        // TODO
        tokens.seek(-1);
        auto parser = new ExpressionParser(tokens);
        parser.until ~= new Token(tokens.current.line,
                tokens.current.lineOffset, TokenType.Symbol, "]");

        elements = parser.run();

        tokens.requireSymbol("]");

        // TODO: shouldn't need this imo
        if (pointerDepth == 0)
        {
            pointerDepth = 1;
        }
    }

    return new IncompleteType(typeName, typeParams, pointerDepth=pointerDepth,
            elements=elements, moduleName=moduleName);
}

// Try to parse a type signature (like "u64* x"), returning null if it can't
// be done.
TypeSignature parseTypeSignature(TokenFeed tokens)
{
    auto type = parseType(tokens);
    auto name = tokens.expectWord().value;

    return new TypeSignature(type, name);
}

Break parseBreak(TokenFeed tokens)
{
    auto line = tokens.current.line;

    tokens.expectWord("break");
    tokens.requireSymbol(";");

    return new Break(line);
}

Continue parseContinue(TokenFeed tokens)
{
    auto line = tokens.current.line;

    tokens.expectWord("continue");
    tokens.requireSymbol(";");

    return new Continue(line);
}

Defer parseDefer(TokenFeed tokens)
{
    auto line = tokens.current.line;

    tokens.expectWord("defer");

    return new Defer(line, parseStatementBase(tokens));
}

Assignment parseAssignment(TokenFeed tokens)
{
    auto line = tokens.current.line;

    // The expression parser always starts by calling .next()
    Node lvalue;
    try
    {
        lvalue = parseExpression(tokens);
    }
    catch (Throwable ex)
    {
        throw new GrammarMismatch();
    }

    tokens.expectSymbol("=");

    // Expression (rvalue)
    Node expression = null;
    try
    {
        expression = parseExpression(tokens);
    }
    catch (Throwable ex)
    {
        throw new GrammarMismatch();
    }

    tokens.requireSymbol(";");

    return new Assignment(line, lvalue, expression);
}

Statement parseStatement(TokenFeed tokens)
{
    auto line = tokens.current.line;

    auto expression = parseExpression(tokens);

    tokens.requireSymbol(";");

    return new Statement(line, expression);
}

Return parseReturn(TokenFeed tokens)
{
    auto start = tokens.current;

    tokens.expectWord("return");

    if (tokens.nextIf(TokenType.Symbol, ";"))
    {
        return new Return(null, null);
    }

    auto ret = new Return(start.line, parseExpression(tokens));
    tokens.requireSymbol(";");
    return ret;
}

While parseWhile(TokenFeed tokens)
{
    auto start = tokens.current;

    tokens.expectWord("while");
    auto block = parseConditionalBlock(tokens);

    return new While(start.line, block.conditional, block.block);
}

If parseIf(TokenFeed tokens)
{
    auto startToken = tokens.expectWord("if");

    auto ifBlock = parseConditionalBlock(tokens);
    ConditionalBlock[] elseIfBlocks;
    StatementBase elseBlock = null;

    while (tokens.nextIf(TokenType.Word, "else"))
    {
        // "else if" block
        if (tokens.nextIf(TokenType.Word, "if"))
        {
            elseIfBlocks ~= parseConditionalBlock(tokens);

            continue;
        }

        // "else" block
        elseBlock = parseStatementBase(tokens);
        break;
    }

    return new If(startToken.line, ifBlock, elseIfBlocks, elseBlock);
}

// Parse a conditional expression followed by a statement or block
ConditionalBlock parseConditionalBlock(TokenFeed tokens)
{
    auto line = tokens.current.line;

    if (!tokens.matchSymbol("("))
    {
        throw new SyntaxError("Expected a conditional expression",
                tokens.current);
    }

    auto conditional = parseExpressionParenthesis(tokens);
    auto block = parseStatementBase(tokens);

    return new ConditionalBlock(line, conditional, block);
}

// This represents either an unparsed lexer Token or a parsed AST Node
class ParserItem
{
    Token token;
    Node node;

    // Represents the function name in a call
    bool functionName;

    // Stores the module name in a qualified function call
    string moduleName;

    // Represents the first open parenthesis in a call
    bool parameterListStart;

    Type[] typeParameters;

    this(Token token)
    {
        this.token = token;
    }

    this(Node node)
    {
        this.node = node;
    }

    bool isToken()
    {
        return this.token !is null;
    }

    bool isNode()
    {
        return !this.isToken();
    }

    override string toString()
    {
        if (this.isToken())
        {
            return format("Token(%s)", this.token);
        }

        return format("Node(%s)", this.node);
    }
}

class ParserItemStack
{
    ParserItem[] stack;

    void push(ParserItem i)
    {
        this.stack ~= i;
    }

    void push(Token t)
    {
        this.push(new ParserItem(t));
    }

    void push(Node n)
    {
        this.push(new ParserItem(n));
    }

    int len()
    {
        return to!int(this.stack.length);
    }

    bool empty()
    {
        return this.len() == 0;
    }

    ParserItem peek(int index)
    {
        int half = this.len() / 2;

        index -= half;
        index *= -1;
        index += half;

        if (this.stack.length % 2 == 0)
        {
            index--;
        }

        return this.stack[index];
    }

    ParserItem pop()
    {
        auto ret = this.peek(0);
        this.stack.length--;
        return ret;
    }
}

Node parseToken(Token t)
{
    switch (t.type)
    {
        case TokenType.Integer:
            return new U64Literal(to!ulong(t.value));
        case TokenType.Word:
            switch (t.value)
            {
                case "true":
                    return new BoolLiteral(true);
                case "false":
                    return new BoolLiteral(false);
                default:
                    return new Binding(null, t.value);
            }
        case TokenType.DoubleQuote:
            return new StringLiteral(t.value);
        case TokenType.SingleQuote:
            if (t.value.length != 1)
            {
                throw new Exception(format("Invalid character literal: %s",
                                           t.value));
            }
            return new U8Literal(t.value[0]);
        default:
            throw new Exception(format("Unrecognized token type: %s", t.type));
    }
}

// If a ParserItem is a Node, return it. If it's a Token, parse it into a Node.
Node getOrParseNode(ParserItem i)
{
    if (i.isNode())
    {
        return i.node;
    }

    return parseToken(i.token);
}

class ExpressionParser
{
    TokenFeed input;

    Token current;

    ParserItemStack output;
    ParserItemStack operators;

    // Tokens to mark the end of an expression
    Token[] until;

    this(TokenFeed input)
    {
        this.input = input;

        this.output = new ParserItemStack();
        this.operators = new ParserItemStack();

        this.until ~= new Token(null, 0, TokenType.Symbol, ";");
        this.until ~= new Token(null, 0, TokenType.Symbol, "=");
    }

    void printState()
    {
        if (!verbose)
        {
            return;
        }

        writefln("----------------------------------------------");
        writefln("current: %s", this.current);

        writefln("output:");
        for (int i = 0; i < this.output.len(); i++)
        {
            writefln("\t%s", this.output.peek(i));
        }

        writefln("operators:");
        for (int i = 0; i < this.operators.len(); i++)
        {
            writefln("\t%s", this.operators.peek(i));
        }
    }

    // Get the number of open parenthesis on the operator stack
    int parenthesisDepth()
    {
        int ret = 0;

        foreach (item; this.operators.stack)
        {
            if (item.isToken() && item.token.match(TokenType.Symbol, "("))
            {
                ret++;
            }
        }

        return ret;
    }

    // Pushes a new item onto the output stack, dealing with special cases
    void outputPush(ParserItem i)
    {
        if (this.consumeCastComplete(i))
        {
            return;
        }

        this.output.push(i);
    }

    void consume()
    {
        auto current = this.current;

        if (this.operators.len() == 0)
        {
            return;
        }

        OperatorType operator;
        try
        {
            operator = operatorType(this.operators.peek(0).token.value);
        }
        catch (Exception)
        {
            throw new SyntaxError(
                "Unrecognized operator",
                current.value, current.line, current.lineOffset);
        }

        int inputCount;
        try
        {
            inputCount = operatorInputCount(operator);
        }
        catch (Exception)
        {
            throw new SyntaxError(
                "Unknown number of operands for operator",
                current.value, current.line, current.lineOffset);
        }

        if (this.output.len() < inputCount)
        {
            throw new SyntaxError(
                format(
                    "Unexpected number of operands (got %d, expected %d) near",
                    this.output.len(), inputCount),
                current.value, current.line, current.lineOffset);
        }

        this.operators.pop();
        Node op = null;

        if (inputCount == 2)
        {
            auto rightItem = this.output.pop();
            auto leftItem = this.output.pop();

            auto right = getOrParseNode(rightItem);
            auto left = getOrParseNode(leftItem);

            if (operator == OperatorType.DotAccessor)
            {
                // Check for a member call
                auto call = cast(Call)rightItem.node;
                if (call !is null)
                {
                    op = new MethodCall(left, call.functionName,
                            call.parameters);
                }
                else
                {
                    op = new DotAccessor(left, rightItem.token.value);
                }
            }
            else
            {
                op = new Operator(left, operator, right);
            }
        }
        else if (inputCount == 1)
        {
            auto rightItem = this.output.pop();
            auto right = getOrParseNode(rightItem);

            switch (operator)
            {
                case OperatorType.Dereference:
                    op = new Dereference(right);
                    break;
                default:
                    throw new SyntaxError("Unrecognized operator",
                        current.value, current.line, current.lineOffset);
            }
        }
        else
        {
            throw new Exception("Unrecognized inputCount");
        }

        this.outputPush(new ParserItem(op));
    }

    // Function calls can start like "func(", "mod::func(", "func<T>(", or
    // "mod::func<T>("
    bool startFunctionCall()
    {
        int rewindTarget = this.input.index;
        bool rewind()
        {
            this.input.index = rewindTarget;
            return false;
        }

        auto token0 = this.input.current;

        if (token0.type != TokenType.Word)
        {
            return rewind();
        }

        if (!this.input.next())
        {
            return rewind();
        }

        Token moduleToken = null;
        Token functionToken = null;

        if (this.input.current.match(TokenType.Symbol, "::"))
        {
            if (!this.input.next())
            {
                return rewind();
            }

            moduleToken = token0;
            functionToken = this.input.current;

            if (!this.input.next())
            {
                return rewind();
            }
        }
        else
        {
            functionToken = token0;
        }

        Type[] typeParameters = tryParse(this.input,
                &parseConcreteTypeParams);
        if (typeParameters is null)
        {
            typeParameters = [];
        }

        if (!this.input.current.match(TokenType.Symbol, "("))
        {
            return rewind();
        }

        auto nameToken = new ParserItem(functionToken);
        nameToken.functionName = true;
        nameToken.typeParameters = typeParameters;
        if (moduleToken !is null)
        {
            nameToken.moduleName = moduleToken.value;
        }
        this.outputPush(nameToken);

        auto startList = new ParserItem(this.input.current);
        startList.parameterListStart = true;
        this.operators.push(startList);

        return true;
    }

    void consumeFunctionCall()
    {
        Node[] parameters;

        while (true)
        {
            auto topOutput = this.output.pop();

            // The function name marks the beginning of the function call
            if (topOutput.isToken() &&
                topOutput.token.type == TokenType.Word &&
                topOutput.functionName)
            {
                // Push the new call onto the output stack
                auto call = new Call(topOutput.moduleName,
                            topOutput.token.value, parameters,
                            topOutput.typeParameters);

                this.outputPush(new ParserItem(call));

                break;
            }

            insertInPlace(parameters, 0, getOrParseNode(topOutput));
        }
    }

    bool consumeSizeOf()
    {
        if (!this.input.current.match(TokenType.Word, "sizeof"))
        {
            return false;
        }

        if (!this.input.next() ||
            !this.input.current.match(TokenType.Symbol, "("))
        {
            throw new Exception("Expected open parenthesis after sizeof");
        }

        if (!this.input.next())
        {
            throw new Exception("Expected a type in sizeof");
        }

        auto type = parseType(this.input);

        if (!this.input.current.match(TokenType.Symbol, ")"))
        {
            throw new Exception("Expected close parenthesis after sizeof");
        }

        this.outputPush(new ParserItem(new SizeOf(type)));

        return true;
    }

    // A cast start looks like '(bool)' and isn't completed until the target
    // is set by a subsequent target (the thing to cast to that type).
    bool consumeCastStart()
    {
        auto rewindTarget = this.input.index;
        bool rewind()
        {
            this.input.index = rewindTarget;
            return false;
        }

        // Check for open parenthesis
        auto cur = this.input.current;

        if (!cur.match(TokenType.Symbol, "("))
        {
            return rewind();
        }

        // Check for a valid type name
        if (!this.input.next() || this.input.current.type != TokenType.Word)
        {
            return rewind();
        }

        auto castType = parseType(this.input);
        if (castType is null)
        {
            return rewind();
        }

        // Check for close parenthesis
        if (!this.input.current.match(TokenType.Symbol, ")"))
        {
            return rewind();
        }

        // TODO: shouldn't need this
        if (this.input.peek(1).match(TokenType.Symbol, "{"))
        {
            return rewind();
        }

        this.outputPush(new ParserItem(new Cast(castType, null)));

        return true;
    }

    // A cast is completed when the top of the output stack has an incomplete
    // cast and the new item being pushed onto the stack is something that can
    // be a cast target.
    bool consumeCastComplete(ParserItem newItem)
    {
        if (this.output.stack.length == 0)
        {
            return false;
        }

        // Check for an incomplete cast
        auto maybeCast = this.output.peek(0);

        writeln("maybeCast: ", maybeCast);
        if (maybeCast is null || !maybeCast.isNode())
        {
            return false;
        }

        auto castNode = cast(Cast)maybeCast.node;
        if (castNode is null || castNode.target !is null)
        {
            return false;
        }

        // Check if newItem is castable
        if (newItem.isToken() &&
            (newItem.token.type == TokenType.Integer ||
             newItem.token.type == TokenType.Word))
        {
            castNode.target = parseToken(newItem.token);
            castNode.target.parent = castNode;
            return true;
        }

        return false;
    }

    // Check for the end of the expression, returning true and consuming any
    // remaining operators if the end is found.
    bool checkForEnd()
    {
        bool end = false;

        foreach (unt; this.until)
        {
            if (this.current.match(unt))
            {
                end = true;
                break;
            }
        }

        if (!end)
        {
            return false;
        }

        while (this.operators.len() > 0)
        {
            this.consume();
        }

        return true;
    }

    bool consumeOperator()
    {
        this.current = this.input.current;

        if (current.type != TokenType.Symbol)
        {
            return false;
        }

        try
        {
            int cur, prev;
            try
            {
                cur = operatorPrecedence(operatorType(current.value));
                prev = operatorPrecedence(operatorType(
                    this.operators.peek(0).token.value));
            }
            catch (Exception)
            {
                auto current = this.current;
                throw new SyntaxError("Unknown precedence for",
                    current.value, current.line, current.lineOffset);
            }

            if (prev >= cur)
            {
                this.consume();
                return true;
            }
        }
        catch (Throwable)
        {
            return false;
        }

        return false;
    }

    bool next()
    {
        auto previous = this.input.current;
        if (!this.input.next())
        {
            throw new SyntaxError("Unexpected",
                "EOF", current.line, current.lineOffset);
        }

        this.current = this.input.current;
        this.printState();

        if (this.checkForEnd())
        {
            return false;
        }

        // Consume as many operators as possible
        while (this.consumeOperator()) {}

        // Detect sizeof
        if (this.consumeSizeOf())
        {
            return true;
        }

        // Check for the beginning of a function call
        if (this.startFunctionCall())
        {
            return true;
        }

        // Detect reference: word preceded by ampersand
        if (current.type == TokenType.Word &&
            this.operators.stack.length > 0)
        {
            auto topOperator = this.operators.peek(0);

            if (topOperator.isToken() &&
                topOperator.token.match(TokenType.Symbol, "&"))
            {
                this.output.push(new ParserItem(new Reference(
                            parseToken(current))));

                this.operators.pop();

                return true;
            }
        }

        // Detect dereference: asterisk symbol preceded by either nothing,
        // a close parenthesis, or an operator.
        if (current.match(TokenType.Symbol, "*"))
        {
            auto previousToken = this.input.peek(-1);

            if (this.output.len() == 0 && this.operators.len() == 0 ||
                previousToken.match(TokenType.Symbol, "(") ||
                previousToken.match(TokenType.Symbol, "&&") ||
                previousToken.match(TokenType.Symbol, "||") ||
                previousToken.match(TokenType.Symbol, "!=") ||
                previousToken.match(TokenType.Symbol, "==") ||
                previousToken.match(TokenType.Symbol, "<=") ||
                previousToken.match(TokenType.Symbol, ">=") ||
                previousToken.match(TokenType.Symbol, "<") ||
                previousToken.match(TokenType.Symbol, ">") ||
                previousToken.match(TokenType.Symbol, "+") ||
                previousToken.match(TokenType.Symbol, "-") ||
                previousToken.match(TokenType.Symbol, "*") ||
                previousToken.match(TokenType.Symbol, "/"))
            {
                // Rewrite as a dereference
                auto rewritten = current.clone();
                rewritten.value = "dereference";
                this.operators.push(new ParserItem(rewritten));

                return true;
            }
        }

        if (current.type != TokenType.Symbol)
        {
            this.outputPush(new ParserItem(current));
            return true;
        }

        // Detect cast: open parenthesis, type, close parenthesis
        if (this.consumeCastStart())
        {
            return true;
        }

        // Hack to allow indexing a struct member
        if (current.value == "[")
        {
            while (this.operators.len() > 0)
            {
                auto topOperator = this.operators.peek(0);

                if (!topOperator.isToken() ||
                    !topOperator.token.match(TokenType.Symbol, "."))
                {
                    break;
                }

                this.consume();
            }
        }

        // Handle end of indexer
        if (current.value == "]")
        {
            // Consume operators until the beginning of the bracket
            while (true)
            {
                auto topOperator = this.operators.peek(0);

                if (topOperator.isToken() &&
                    topOperator.token.match(TokenType.Symbol, "["))
                {
                    auto valueItem = this.output.pop();
                    auto indexerItem = this.output.pop();

                    Node valueNode;
                    if (valueItem.isToken())
                    {
                        valueNode = parseToken(valueItem.token);
                    }
                    else
                    {
                        valueNode = valueItem.node;
                    }

                    Node indexerNode;
                    if (indexerItem.isToken())
                    {
                        indexerNode = parseToken(indexerItem.token);
                    }
                    else
                    {
                        indexerNode = indexerItem.node;
                    }

                    auto indexer = new Indexer(indexerNode, valueNode);

                    this.output.push(indexer);

                    this.operators.pop();
                    return true;
                }

                this.consume();
            }
        }

        if (current.value == "," || current.value == ")")
        {
            // Consume operators until the beginning of the parameter list
            while (true)
            {
                auto topOperator = this.operators.peek(0);

                if (topOperator.isToken() &&
                    topOperator.token.type == TokenType.Symbol &&
                    topOperator.token.value == "(")
                {
                    // Don't clean up the function call yet if we're only on a
                    // comma
                    if (current.value == ",")
                    {
                        return true;
                    }

                    // Function call?
                    if (topOperator.parameterListStart)
                    {
                        this.consumeFunctionCall();
                    }

                    // Pop off the open parenthesis
                    this.operators.pop();
                    return true;
                }

                this.consume();
            }
        }

        this.operators.push(current);
        return true;
    }

    Node run()
    {
        Token current = this.input.current;

        if (verbose)
        {
            writeln("\nExpression parser starting: ", this.input);
        }

        while (this.next())
        {
            current = this.input.current;
        }

        auto len = this.output.len();
        if (len != 1)
        {
            // TODO: clarify what this implies.
            throw new SyntaxError(
                format("Expected one (got %d) token to be left near", len),
                current.value, current.line, current.lineOffset);
        }

        return getOrParseNode(this.output.pop());
    }
}

Node parseExpression(TokenFeed tokens)
{
    // TODO: yuck
    tokens.seek(-1);
    return new ExpressionParser(tokens).run();
}

// Parse an expression in parenthesis, ending on the first close parenthesis
// with no open parenthesis on the stack
Node parseExpressionParenthesis(TokenFeed tokens)
{
    Token current = tokens.current;

    // TODO: yucky
    tokens.seek(-1);
    auto parser = new ExpressionParser(tokens);

    while (true)
    {
        if (!parser.next())
        {
            break;
        }

        current = tokens.current;

        // Look for the terminating close parenthesis
        if (parser.input.peek(1) !is null &&
            parser.input.peek(1).match(TokenType.Symbol, ")") &&
            parser.parenthesisDepth() == 1)
        {
            // Consume the last parenthesis
            parser.next();
            tokens.next();

            if (parser.output.len() != 1)
            {
                throw new SyntaxError(
                    "Expected one token to be left near",
                    current.value, current.line, current.lineOffset);
            }

            return getOrParseNode(parser.output.pop());
        }
    }

    throw new SyntaxError(
        "Expected a closing parenthesis near",
        current.value, current.line, current.lineOffset);
}
