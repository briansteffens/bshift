import std.stdio;
import std.format;
import std.conv;
import std.array;
import std.file;
import std.algorithm;

import globals;
import terminal;
import lexer;
import ast;
import expressions;
import validator;

class SyntaxError : Exception
{
    public Token token;

    this(string message, Token token)
    {
        this.token = token;
        super(message);
    }
}

class UnexpectedEOF : SyntaxError
{
    this(Token token)
    {
        super("The end of the file was reached unexpectedly.", token);
    }
}

// This is raised when, say, parseStruct is called on an if statement: the
// grammar doesn't match but it doesn't necessarily mean the tokens are
// invalid syntax.
class GrammarMismatch : Exception
{
    Token token;

    this(Token token, string efile = __FILE__, size_t eline = __LINE__)
    {
        super("The tokens could not be parsed as the given grammar rule",
                efile, eline);

        this.token = token;
    }
}

// This is raised when the parser malfunctions.
class ParserException : Exception
{
    this(string s)
    {
        super(s);
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

    private void requireNotEOF()
    {
        if (eof)
        {
            throw new UnexpectedEOF(tokens[$-1]);
        }
    }

    bool next()
    {
        requireNotEOF();
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
        requireNotEOF();
        return this.tokens[this.index];
    }

    // If the current token matches the given criteria, return it and advance
    // the reader. Otherwise, throw GrammarMismatch.
    Token expect(TokenType type, string value = null)
    {
        auto ret = this.current;

        if (!ret.match(type, value))
        {
            throw new GrammarMismatch(ret);
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
        if (eof)
        {
            throw new SyntaxError("Unexpected end-of-file reached", peek(-1));
        }

        if (!match(type, value))
        {
            if (errorMessage is null)
            {
                errorMessage = format("At [%s], expected '%s'", current,
                        value);
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

    Token requireAnyWord(string errorMessage = null)
    {
        return requireWord(null, errorMessage);
    }

    Token requireSymbol(string value = null, string errorMessage = null)
    {
        return require(TokenType.Symbol, value, errorMessage);
    }

    // Checks if the current token matches the criteria
    bool match(TokenType type, string value = null)
    {
        return inBounds(index) && current.match(type, value);
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

    // Turn off verbose mode for imports, it's a ton of extra noise for little
    // benefit. Restore it afterward.
    bool verboseOld = verbose;
    verbose = false;

    // Parse the import
    auto parsed = parse(moduleName, lex(filename, readText(filename)));

    verbose = verboseOld;

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

// Parse qualified import: import io;
Import[] parseImport(TokenFeed tokens)
{
    auto start = tokens.expectWord("import");
    auto moduleName = tokens.requireAnyWord(
            "Expected module or symbol name to import").value;
    tokens.expectSymbol(";");

    return processImport(start, moduleName);
}

// Parse unqualified import: import length, reverse from cstring;
Import[] parseImportFrom(TokenFeed tokens)
{
    auto start = tokens.expectWord("import");

    string[] symbols;

    while (true)
    {
        symbols ~= tokens.requireAnyWord(
                "Expected symbol name to import").value;

        if (tokens.nextIfWord("from"))
        {
            break;
        }

        tokens.requireSymbol(",", "Expected ',' or 'from'");
    }

    auto moduleName = tokens.requireAnyWord(
            "Expected module to import from").value;

    tokens.requireSymbol(";");

    return processImport(start, moduleName, symbols);
}

Module parse(string name, string source)
{
    return parse(name, lex(name, source));
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

        auto struct_ = tryParse(tokens, &parseStruct);
        if (struct_ !is null)
        {
            auto structTemplate = cast(StructTemplate)struct_;
            if (structTemplate !is null)
            {
                structTemplates ~= structTemplate;
            }
            else
            {
                structs ~= struct_;
            }
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

        throw new SyntaxError("Can't parse grammar near", current);
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

    if (verbose)
    {
        writeln("bshift validated ast ------------------------------------\n");
        writeln(ret);
    }

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
    tokens.requireSymbol("{");

    TypeSignature[] members;

    while (!tokens.nextIfSymbol("}"))
    {
        void raise(Token t)
        {
            throw new SyntaxError("Expected a struct member", t);
        }

        try
        {
            members ~= parseTypeSignature(tokens);
        }
        catch (GrammarMismatch e)
        {
            raise(e.token);
        }
        catch (UnexpectedEOF e)
        {
            raise(e.token);
        }

        tokens.requireSymbol(";");
    }

    return members;
}

Struct parseStruct(TokenFeed tokens)
{
    bool exported = tokens.nextIfWord("export");

    tokens.expectWord("struct");

    auto name = tokens.requireAnyWord("Expected struct name").value;

    TypeParameter[] typeParams;
    try
    {
        typeParams = parseTypeParams(tokens);
    }
    catch (GrammarMismatch)
    {
    }

    // Parse struct members
    auto members = parseStructMembers(tokens);

    if (typeParams.length == 0)
    {
        return new Struct(name, members, exported=exported);
    }
    else
    {
        return new StructTemplate(name, typeParams, members,
                exported=exported);
    }
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
    auto containerName = tokens.expectWord();
    auto containerType = new IncompleteType(containerName.value, []);
    containerType.token = containerName;

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

    return new LocalDeclaration(start, typeSignature, expression);
}

Type parseType(TokenFeed tokens)
{
    // Read either the module or type name
    auto start = tokens.current;
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
        parser.until ~= new Token(tokens.current.location, TokenType.Symbol,
                "]");

        elements = parser.run();

        tokens.requireSymbol("]");

        // TODO: shouldn't need this imo
        if (pointerDepth == 0)
        {
            pointerDepth = 1;
        }
    }

    auto ret = new IncompleteType(typeName, typeParams,
            pointerDepth=pointerDepth, elements=elements,
            moduleName=moduleName);
    ret.token = start;
    return ret;
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
    auto start = tokens.current;

    tokens.expectWord("break");
    tokens.requireSymbol(";");

    return new Break(start);
}

Continue parseContinue(TokenFeed tokens)
{
    auto start = tokens.current;

    tokens.expectWord("continue");
    tokens.requireSymbol(";");

    return new Continue(start);
}

Defer parseDefer(TokenFeed tokens)
{
    auto start = tokens.current;

    tokens.expectWord("defer");

    return new Defer(start, parseStatementBase(tokens));
}

Assignment parseAssignment(TokenFeed tokens)
{
    auto start = tokens.current;

    // The expression parser always starts by calling .next()
    Node lvalue;
    try
    {
        lvalue = parseExpression(tokens);
    }
    catch (Throwable ex)
    {
        throw new GrammarMismatch(tokens.current);
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
        throw new GrammarMismatch(tokens.current);
    }

    tokens.requireSymbol(";");

    return new Assignment(start, lvalue, expression);
}

Statement parseStatement(TokenFeed tokens)
{
    auto start = tokens.current;

    auto expression = parseExpression(tokens);

    tokens.requireSymbol(";");

    return new Statement(start, expression);
}

Return parseReturn(TokenFeed tokens)
{
    auto start = tokens.current;

    tokens.expectWord("return");

    if (tokens.nextIf(TokenType.Symbol, ";"))
    {
        return new Return(null, null);
    }

    auto ret = new Return(start, parseExpression(tokens));
    tokens.requireSymbol(";");
    return ret;
}

While parseWhile(TokenFeed tokens)
{
    auto start = tokens.current;

    tokens.expectWord("while");
    auto block = parseConditionalBlock(tokens);

    return new While(start, block.conditional, block.block);
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

    return new If(startToken, ifBlock, elseIfBlocks, elseBlock);
}

// Parse a conditional expression followed by a statement or block
ConditionalBlock parseConditionalBlock(TokenFeed tokens)
{
    auto start = tokens.current;

    if (!tokens.matchSymbol("("))
    {
        throw new SyntaxError("Expected a conditional expression",
                tokens.current);
    }

    auto conditional = parseExpressionParenthesis(tokens);
    auto block = parseStatementBase(tokens);

    return new ConditionalBlock(start, conditional, block);
}
