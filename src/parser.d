import std.stdio;
import std.format;
import std.conv;
import std.array;
import std.file;
import std.algorithm;

import globals;
import lexer;
import ast;
import validator;

OperatorType parseOperatorType(string input)
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

class TokenFeed
{
    Token[] tokens;
    int index;
    bool started = false;

    this(Token[] tokens)
    {
        this.tokens = tokens;
        this.index = 0;
    }

    bool next()
    {
        if (!this.started)
        {
            this.started = true;
            return true;
        }

        if (this.index >= this.tokens.length - 1)
        {
            return false;
        }

        this.index++;
        return true;
    }

    bool seek(int distance)
    {
        auto changed = this.index + distance;

        if (changed < 0 || changed >= this.tokens.length)
        {
            return false;
        }

        this.index = changed;
        return true;
    }

    Token peek(int distance)
    {
        int target = this.index + distance;

        if (target < 0 || target >= this.tokens.length)
        {
            return null;
        }

        return this.tokens[target];
    }

    Token current()
    {
        return this.tokens[this.index];
    }
}

string[] possibleImportPaths(string moduleName)
{
    string[] ret;

    ret ~= format("%s.bs", moduleName);
    ret ~= format("lib/%s.bs", moduleName);
    ret ~= format("/usr/local/lib/bshift/%s.bs", moduleName);

    return ret;
}

Import[] parseImport(TokenFeed tokens)
{
    auto rewindTarget = tokens.index;
    Import[] rewind()
    {
        tokens.index = rewindTarget;
        return null;
    }

    auto current = tokens.current();
    if (!current.match(TokenType.Word, "import"))
    {
        return rewind();
    }

    if (!tokens.next() || tokens.current().type != TokenType.Word)
    {
        return rewind();
    }

    bool unqualified = false;
    if (tokens.current().value == "unqualified")
    {
        unqualified = true;

        if (!tokens.next())
        {
            return rewind();
        }
    }

    if (current.type != TokenType.Word)
    {
        throw new ProgrammerException("Expected a module name to import near",
            current.value, current.line, current.lineOffset);
    }

    current = tokens.current();
    auto name = current.value;
    tokens.next();

    // Resolve import
    string[] possibilities = possibleImportPaths(name);
    string filename = null;
    foreach (possible; possibilities)
    {
        if (exists(possible))
        {
            filename = possible;
            break;
        }
    }

    if (filename is null)
    {
        throw new ProgrammerException(
            format(
                "Can't find a file (among %s) to match the imported module",
                possibilities),
            current.value, current.line, current.lineOffset);
    }

    // Parse the import
    auto parsed = parse(name, lex(readText(filename), filename));

    FunctionTemplate[] functionTemplates;
    FunctionSignature[] signatures;
    foreach (func; parsed.functions)
    {
        if (!func.signature.exported)
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
        if (s.exported)
        {
            structs ~= s;
        }
    }

    StructTemplate[] structTemplates;
    foreach (st; parsed.structTemplates)
    {
        if (st.exported)
        {
            structTemplates ~= st;
        }
    }

    return [new Import(filename, name, signatures, functionTemplates,
            structs, structTemplates, unqualified=unqualified)]
           ~ parsed.imports;
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
    Token current = tokens.current();

    while (tokens.next())
    {
        current = tokens.current();

        auto imps = parseImport(tokens);
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

        auto ext = parseExtern(tokens);
        if (ext !is null)
        {
            externs ~= ext;
            continue;
        }

        auto structTemplate = parseStructTemplate(tokens);
        if (structTemplate !is null)
        {
            structTemplates ~= structTemplate;
            continue;
        }

        auto struct_ = parseStruct(tokens);
        if (struct_ !is null)
        {
            structs ~= struct_;
            continue;
        }

        auto method = parseMethod(tokens);
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

        auto func = parseFunction(tokens);
        if (func !is null)
        {
            functions ~= func;
            continue;
        }

        auto global = parseGlobal(tokens);
        if (global !is null)
        {
            globals ~= global;
            continue;
        }

        throw new ProgrammerException(
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

    //try
    //{
        validate(ret);
    //}
    //catch (Exception e)
    //{
    //    throw new ProgrammerException(e.msg, current.value, current.line,
      //                                current.lineOffset, e.file, e.line);
    //}

    return ret;
}

Global parseGlobal(TokenFeed tokens)
{
    int rewindTarget = tokens.index;

    auto signature = parseTypeSignature(tokens);
    if (signature is null)
    {
        tokens.index = rewindTarget;
        return null;
    }

    if (!tokens.next())
    {
        tokens.index = rewindTarget;
        return null;
    }

    Node value = null;
    if (tokens.current().match(TokenType.Symbol, "="))
    {
        value = new ExpressionParser(tokens).run();
    }
    else if (!tokens.current().match(TokenType.Symbol, ";"))
    {
        tokens.index = rewindTarget;
        return null;
    }

    return new Global(signature, value);
}

TypeSignature[] parseStructMembers(TokenFeed tokens)
{
    auto rewindTarget = tokens.index;
    TypeSignature[] rewind()
    {
        tokens.index = rewindTarget;
        return null;
    }

    if (!tokens.next() || !tokens.current().match(TokenType.Symbol, "{"))
    {
        return rewind();
    }

    TypeSignature[] members;
    while (tokens.next() && !tokens.current().match(TokenType.Symbol, "}"))
    {
        members ~= parseTypeSignature(tokens);

        // Assignment operator (=) or semi-colon
        if (!tokens.next() || !tokens.current().match(TokenType.Symbol, ";"))
        {
            auto current = tokens.current();
            throw new ProgrammerException(
                "Expected semi-colon near",
                current.value, current.line, current.lineOffset);
        }
    }

    return members;
}

Struct parseStruct(TokenFeed tokens)
{
    auto rewindTarget = tokens.index;
    Struct rewind()
    {
        tokens.index = rewindTarget;
        return null;
    }

    bool exported = false;
    if (tokens.current().match(TokenType.Word, "export"))
    {
        exported = true;
        if (!tokens.next())
        {
            return rewind();
        }
    }

    if (!tokens.current().match(TokenType.Word, "struct"))
    {
        return rewind();
    }

    if (!tokens.next() || tokens.current().type != TokenType.Word)
    {
        throw new Exception("Expected a struct name");
    }

    auto name = tokens.current().value;

    // Parse struct members
    auto members = parseStructMembers(tokens);

    return new Struct(name, members, exported=exported);
}

StructTemplate parseStructTemplate(TokenFeed tokens)
{
    auto rewindTarget = tokens.index;
    StructTemplate rewind()
    {
        tokens.index = rewindTarget;
        return null;
    }

    bool exported = false;
    if (tokens.current().match(TokenType.Word, "export"))
    {
        exported = true;
        if (!tokens.next())
        {
            return rewind();
        }
    }

    if (!tokens.current().match(TokenType.Word, "struct"))
    {
        return rewind();
    }

    if (!tokens.next() || tokens.current().type != TokenType.Word)
    {
        return rewind();
    }

    auto name = tokens.current().value;

    // Template type parameters
    auto typeParams = parseTypeParams(tokens);

    if (typeParams.length == 0)
    {
        return rewind();
    }

    // Parse struct members
    auto members = parseStructMembers(tokens);

    return new StructTemplate(name, typeParams, members, exported=exported);
}

FunctionSignature parseExtern(TokenFeed tokens)
{
    if (!tokens.current().match(TokenType.Word, "extern"))
    {
        return null;
    }

    if (!tokens.next())
    {
        throw new Exception("Expected a function signature after extern");
    }

    TypeParameter[] typeParams;
    auto sig = parseFunctionSignature(tokens, &typeParams);

    if (!tokens.next() || !tokens.current().match(TokenType.Symbol, ";"))
    {
        throw new Exception("Expected a semi-colon after extern");
    }

    return sig;
}

FunctionSignature parseFunctionSignature(TokenFeed tokens,
        TypeParameter[]* typeParams)
{
    if (tokens.current().type != TokenType.Word)
    {
        return null;
    }

    // Check for 'export' keyword
    bool exported = false;
    if (tokens.current().value == "export")
    {
        exported = true;
        if (!tokens.next())
        {
            return null;
        }
    }

    // Parse the return value
    auto type = parseType(tokens);

    // Function name
    if (!tokens.next() || tokens.current().type != TokenType.Word)
    {
        return null;
    }

    auto name = tokens.current().value;

    // Template type parameters
    foreach (typeParam; parseTypeParams(tokens))
    {
        *typeParams ~= typeParam;
    }

    // Parameters
    TypeSignature[] params;
    bool variadic;
    if (!parseParameterList(tokens, &params, &variadic))
    {
        return null;
    }

    return new FunctionSignature(type, name, params, variadic,
            exported=exported);
}

MethodSignature parseMethodSignature(TokenFeed tokens)
{
    // Return value
    auto token = tokens.current();

    if (token.type != TokenType.Word)
    {
        return null;
    }

    auto returnType = parseType(tokens);

    // Container type
    if (!tokens.next() || tokens.current().type != TokenType.Word)
    {
        return null;
    }

    auto containerType = new IncompleteType(tokens.current().value, []);

    // Member operator ::
    if (!tokens.next() || !tokens.current().match(TokenType.Symbol, "::"))
    {
        return null;
    }

    // Function name
    if (!tokens.next() || tokens.current().type != TokenType.Word)
    {
        return null;
    }

    auto functionName = tokens.current().value;

    TypeSignature[] params;
    bool variadic;
    if (!parseParameterList(tokens, &params, &variadic))
    {
        return null;
    }

    return new MethodSignature(returnType, containerType, functionName,
                               params, variadic);
}

TypeParameter parseTypeParam(TokenFeed tokens)
{
    auto name = tokens.current();

    if (name.type != TokenType.Word)
    {
        return null;
    }

    return new TypeParameter(name.value);
}

// Parse template type params, as in: <T, U>
TypeParameter[] parseTypeParams(TokenFeed tokens)
{
    int rewindTarget = tokens.index;
    TypeParameter[] rewind()
    {
        tokens.index = rewindTarget;
        return [];
    }

    if (!tokens.next())
    {
        return rewind();
    }

    // Open bracket
    if (!tokens.current.match(TokenType.Symbol, "<"))
    {
        return rewind();
    }

    TypeParameter[] ret;

    while (tokens.next())
    {
        auto token = tokens.current();

        // Close bracket - end of type param list
        if (token.match(TokenType.Symbol, ">"))
        {
            return ret;
        }

        auto typeParam = parseTypeParam(tokens);
        if (typeParam is null)
        {
            return rewind();
        }
        ret ~= typeParam;

        // Commas separate parameters
        auto next = tokens.peek(1);
        if (next !is null &&
            next.type == TokenType.Symbol &&
            next.value == ",")
        {
            tokens.next();
        }
    }

    throw new Exception("Type parameter list ended unexpectedly");
}

// Parse concrete template type params, as in: <u64, u8>
Type[] parseConcreteTypeParams(TokenFeed tokens)
{
    int rewindTarget = tokens.index;
    Type[] rewind()
    {
        tokens.index = rewindTarget;
        return [];
    }

    if (!tokens.next())
    {
        return rewind();
    }

    // Open bracket
    if (!tokens.current.match(TokenType.Symbol, "<"))
    {
        return rewind();
    }

    Type[] ret;

    while (tokens.next())
    {
        auto token = tokens.current();

        // Close bracket - end of type param list
        if (token.match(TokenType.Symbol, ">"))
        {
            return ret;
        }

        auto type = parseType(tokens);
        if (type is null)
        {
            return rewind();
        }
        ret ~= type;

        // Commas separate parameters
        auto next = tokens.peek(1);
        if (next !is null &&
            next.type == TokenType.Symbol &&
            next.value == ",")
        {
            tokens.next();
        }
    }

    throw new Exception("Concrete type parameter list ended unexpectedly");
}

bool parseParameterList(TokenFeed tokens, TypeSignature[]* params,
                        bool* variadic)
{
    // Open parenthesis
    if (!tokens.next() || !tokens.current().match(TokenType.Symbol, "("))
    {
        return false;
    }

    *variadic = false;

    while (tokens.next())
    {
        auto token = tokens.current();

        // Variadic
        if (token.match(TokenType.Symbol, "..."))
        {
            if (!tokens.next() ||
                !tokens.current().match(TokenType.Symbol, ")"))
            {
                throw new Exception(
                        "Variadic ... symbol must be the last entry in a " ~
                        "parameter list");
            }

            *variadic = true;
            break;
        }

        // End of parameter list
        if (token.match(TokenType.Symbol, ")"))
        {
            break;
        }

        *params ~= parseTypeSignature(tokens);

        // Commas separate parameters
        auto next = tokens.peek(1);
        if (next !is null &&
            next.type == TokenType.Symbol &&
            next.value == ",")
        {
            tokens.next();
        }
    }

    return true;
}

Function parseFunction(TokenFeed tokens)
{
    auto rewindTarget = tokens.index;

    TypeParameter[] typeParams;
    auto sig = parseFunctionSignature(tokens, &typeParams);
    if (sig is null)
    {
        tokens.index = rewindTarget;
        return null;
    }

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
    auto rewindTarget = tokens.index;

    auto sig = parseMethodSignature(tokens);
    if (sig is null)
    {
        tokens.index = rewindTarget;
        return null;
    }

    auto functionBody = parseBlock(tokens);

    return new Method(sig, functionBody);
}

Block parseBlock(TokenFeed tokens)
{
    // Open bracket
    if (!tokens.next())
    {
        throw new Exception("Expected a function body");
    }

    auto token = tokens.current();

    if (token.type != TokenType.Symbol || token.value != "{")
    {
        throw new Exception("Expected a function body");
    }

    StatementBase[] statements;

    while (tokens.next())
    {
        auto current = tokens.current();

        // End of function body
        if (current.type == TokenType.Symbol && current.value == "}")
        {
            break;
        }

        statements ~= parseStatementBase(tokens);
    }

    return new Block(statements);
}

StatementBase parseStatementBase(TokenFeed tokens)
{
    auto current = tokens.current();

    // Start of block
    if (current.match(TokenType.Symbol, "{"))
    {
        if (!tokens.seek(-1))
        {
            throw new Exception("Can't rewind?");
        }

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
                auto localDeclaration = parseLocalDeclaration(tokens);
                if (localDeclaration !is null)
                {
                    return localDeclaration;
                }
        }
    }

    auto assignment = parseAssignment(tokens);
    if (assignment !is null)
    {
        return assignment;
    }

    auto statement = parseStatement(tokens);
    if (statement !is null)
    {
        return statement;
    }

    throw new Exception(format(
            "Can't parse grammar starting with \"%s\" at %s:%d,%d",
            current.value, current.line.file, current.line.number,
            current.lineOffset));
}

// Try to parse a local declaration (like "u64 x = 3;") and return null if it
// can't be done.
LocalDeclaration parseLocalDeclaration(TokenFeed tokens)
{
    auto line = tokens.current().line;
    auto rewindTarget = tokens.index;

    // Try to parse the lvalue
    auto typeSignature = parseTypeSignature(tokens);
    if (typeSignature is null)
    {
        tokens.index = rewindTarget;
        return null;
    }

    // Assignment operator (=) or semi-colon
    if (!tokens.next())
    {
        throw new Exception("Expected assignment operator (=) or semi-colon");
    }

    auto current = tokens.current();

    // Try to parse the rvalue if there is one
    Node expression = null;

    // Assignment
    if (current.match(TokenType.Symbol, "="))
    {
        expression = new ExpressionParser(tokens).run();
    }
    // Constructor
    else if (current.match(TokenType.Symbol, "("))
    {
        // Kinda weird - rewind so the expression parser sees the local name.
        // So "point p(3, 7)" gets parsed as the call "p(3, 7)". Then we can
        // replace "p" with the real constructor to be called. Just hacking
        // around the expression parser which is getting pretty due for a
        // refactor.
        tokens.index -= 2;
        auto parsed = new ExpressionParser(tokens).run();
        auto call = cast(Call)parsed;
        if (call is null)
        {
            throw new Exception("Bad constructor call");
        }
        auto container = new Binding(typeSignature, call.functionName);
        auto constructor = new MethodCall(container, "construct",
                call.parameters);
        expression = constructor;
    }
    else if (!current.match(TokenType.Symbol, ";"))
    {
        throw new Exception("Expected assignment operator (=) or semi-colon");
    }

    return new LocalDeclaration(line, typeSignature, expression);
}

Type parseType(TokenFeed tokens)
{
    int rewindTarget = tokens.index;
    Type rewind()
    {
        tokens.index = rewindTarget;
        return null;
    }

    // Read either the module or type name
    if (tokens.current().type != TokenType.Word)
    {
        return rewind();
    }

    auto current = tokens.current();
    string moduleName = null;
    string typeName = tokens.current().value;

    // Detect scope operator, which means we have a module and a type name
    if (tokens.peek(1).match(TokenType.Symbol, "::"))
    {
        if (!tokens.next() || !tokens.next() ||
            tokens.current().type != TokenType.Word)
        {
            return rewind();
        }

        moduleName = typeName;
        typeName = tokens.current().value;
    }

    // Read type parameters
    auto typeParams = parseConcreteTypeParams(tokens);

    // Read pointer symbol
    bool pointer = false;
    if (tokens.peek(1).match(TokenType.Symbol, "*"))
    {
        pointer = true;
        if (!tokens.next())
        {
            return rewind();
        }
    }

    // Read array element count
    Node elements = null;
    if (tokens.peek(1).match(TokenType.Symbol, "["))
    {
        if (!tokens.next())
        {
            return rewind();
        }

        auto parser = new ExpressionParser(tokens);
        parser.until ~= new Token(current.line, current.lineOffset,
                TokenType.Symbol, "]");
        elements = parser.run();

        pointer = true;
    }

    return new IncompleteType(typeName, typeParams, pointer=pointer,
            elements=elements, moduleName=moduleName);
}

// Try to parse a type signature (like "u64* x"), returning null if it can't
// be done.
TypeSignature parseTypeSignature(TokenFeed tokens)
{
    auto type = parseType(tokens);

    // New local name
    if (!tokens.next() || tokens.current().type != TokenType.Word)
    {
        return null;
    }

    return new TypeSignature(type, tokens.current().value);
}

Break parseBreak(TokenFeed tokens)
{
    auto line = tokens.current().line;
    tokens.next();
    return new Break(line);
}

Continue parseContinue(TokenFeed tokens)
{
    auto line = tokens.current().line;
    tokens.next();
    return new Continue(line);
}

Defer parseDefer(TokenFeed tokens)
{
    auto line = tokens.current().line;
    tokens.next();
    return new Defer(line, parseStatementBase(tokens));
}

Assignment parseAssignment(TokenFeed tokens)
{
    auto line = tokens.current().line;
    auto rewindTarget = tokens.index;

    // parseExpression always starts by calling .next()
    tokens.seek(-1);
    Node lvalue;
    try
    {
        lvalue = new ExpressionParser(tokens).run();
    }
    catch (Throwable ex)
    {
        tokens.index = rewindTarget;
        return null;
    }

    auto current = tokens.current();

    if (!current.match(TokenType.Symbol, "="))
    {
        tokens.index = rewindTarget;
        return null;
    }

    // Expression (rvalue)
    Node expression = null;
    try
    {
        expression = new ExpressionParser(tokens).run();
    }
    catch (Throwable ex)
    {
        tokens.index = rewindTarget;
        return null;
    }

    return new Assignment(line, lvalue, expression);
}

Statement parseStatement(TokenFeed tokens)
{
    auto line = tokens.current().line;

    // parseExpression always starts by calling .next()
    tokens.seek(-1);
    auto expression = new ExpressionParser(tokens).run();

    return new Statement(line, expression);
}

Return parseReturn(TokenFeed tokens)
{
    auto line = tokens.current().line;

    if (tokens.peek(1).match(TokenType.Symbol, ";"))
    {
        tokens.next();
        return new Return(null, null);
    }

    return new Return(line, new ExpressionParser(tokens).run());
}

While parseWhile(TokenFeed tokens)
{
    auto line = tokens.current().line;

    auto block = parseConditionalBlock(tokens);

    return new While(line, block.conditional, block.block);
}

If parseIf(TokenFeed tokens)
{
    auto startToken = tokens.current();

    auto ifBlock = parseConditionalBlock(tokens);
    ConditionalBlock[] elseIfBlocks;
    StatementBase elseBlock = null;

    while (true)
    {
        // No more else blocks
        auto elseKeyword = tokens.peek(1);
        if (elseKeyword is null || !elseKeyword.match(TokenType.Word, "else"))
        {
            break;
        }

        tokens.next();

        // "else if" block
        auto ifKeyword = tokens.peek(1);
        if (ifKeyword is null || ifKeyword.match(TokenType.Word, "if"))
        {
            tokens.next();

            elseIfBlocks ~= parseConditionalBlock(tokens);

            continue;
        }

        // "else" block
        tokens.next();
        elseBlock = parseStatementBase(tokens);
        break;
    }

    return new If(startToken.line, ifBlock, elseIfBlocks, elseBlock);
}

// Parse a conditional expression followed by a statement or block
ConditionalBlock parseConditionalBlock(TokenFeed tokens)
{
    auto line = tokens.current().line;

    // Make sure there's an open parenthesis
    auto next = tokens.peek(1);

    if (next is null || !next.match(TokenType.Symbol, "("))
    {
        throw new Exception("Expected an if conditional");
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
            operator = parseOperatorType(this.operators.peek(0).token.value);
        }
        catch (Exception)
        {
            throw new ProgrammerException(
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
            throw new ProgrammerException(
                "Unknown number of operands for operator",
                current.value, current.line, current.lineOffset);
        }

        if (this.output.len() < inputCount)
        {
            throw new ProgrammerException(
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
                    throw new ProgrammerException("Unrecognized operator",
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

        auto token0 = this.input.current();

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

        if (this.input.current().match(TokenType.Symbol, "::"))
        {
            if (!this.input.next())
            {
                return rewind();
            }

            moduleToken = token0;
            functionToken = this.input.current();

            if (!this.input.next())
            {
                return rewind();
            }
        }
        else
        {
            functionToken = token0;
        }

        Type[] typeParameters = [];
        if (this.input.current().match(TokenType.Symbol, "<"))
        {
            this.input.seek(-1);
            typeParameters = parseConcreteTypeParams(this.input);
            if (!this.input.next())
            {
                return rewind();
            }
        }

        if (!this.input.current().match(TokenType.Symbol, "("))
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

        auto startList = new ParserItem(this.input.current());
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
        if (!this.input.current().match(TokenType.Word, "sizeof"))
        {
            return false;
        }

        if (!this.input.next() ||
            !this.input.current().match(TokenType.Symbol, "("))
        {
            throw new Exception("Expected open parenthesis after sizeof");
        }

        if (!this.input.next())
        {
            throw new Exception("Expected a type in sizeof");
        }

        auto type = parseType(this.input);

        if (!this.input.next() ||
            !this.input.current().match(TokenType.Symbol, ")"))
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
        // Check for open parenthesis
        auto cur = this.input.current();

        if (!cur.match(TokenType.Symbol, "("))
        {
            return false;
        }

        // Check for a valid type name
        cur = this.input.peek(1);

        if (cur.type != TokenType.Word)
        {
            return false;
        }

        Primitive castType;
        try
        {
            castType = parsePrimitive(cur.value);
        }
        catch (Throwable)
        {
            return false;
        }

        // Check for close parenthesis
        cur = this.input.peek(2);

        if (!cur.match(TokenType.Symbol, ")"))
        {
            return false;
        }

        // Found a cast, rewrite these tokens as an incomplete cast
        this.input.next();
        this.input.next();

        // TODO: Allow casting to a complete type signature, not just a
        // primtive
        this.outputPush(new ParserItem(new Cast(new PrimitiveType(castType),
                        null)));

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
        this.current = this.input.current();

        if (current.type != TokenType.Symbol)
        {
            return false;
        }

        try
        {
            int cur, prev;
            try
            {
                cur = operatorPrecedence(parseOperatorType(current.value));
                prev = operatorPrecedence(parseOperatorType(
                    this.operators.peek(0).token.value));
            }
            catch (Exception)
            {
                auto current = this.current;
                throw new ProgrammerException("Unknown precedence for",
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
        auto previous = this.input.current();
        if (!this.input.next())
        {
            throw new ProgrammerException("Unexpected",
                "EOF", current.line, current.lineOffset);
        }

        this.current = this.input.current();
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
        Token current = this.input.current();
        while (this.next())
        {
            current = this.input.current();
        }

        auto len = this.output.len();
        if (len != 1)
        {
            // TODO: clarify what this implies.
            throw new ProgrammerException(
                format("Expected one (got %d) token to be left near", len),
                current.value, current.line, current.lineOffset);
        }

        return getOrParseNode(this.output.pop());
    }
}

// Parse an expression in parenthesis, ending on the first close parenthesis
// with no open parenthesis on the stack
Node parseExpressionParenthesis(TokenFeed tokens)
{
    auto parser = new ExpressionParser(tokens);

    Token current = tokens.current();
    while (parser.next())
    {
        current = tokens.current();
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
                throw new ProgrammerException(
                    "Expected one token to be left near",
                    current.value, current.line, current.lineOffset);
            }

            return getOrParseNode(parser.output.pop());
        }
    }

    throw new ProgrammerException(
        "Expected a closing parenthesis near",
        current.value, current.line, current.lineOffset);
}
