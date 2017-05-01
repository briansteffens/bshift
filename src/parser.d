import std.stdio;
import std.format;
import std.conv;
import std.array;
import std.file;

import globals;
import lexer;
import ast;

OperatorType parseOperatorType(string input)
{
    switch (input)
    {
        case "+":
            return OperatorType.Plus;
        case "-":
            return OperatorType.Minus;
        case "*":
            return OperatorType.Asterisk;
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
        case ".":
            return OperatorType.DotAccessor;
        default:
            throw new Exception(
                    format("Unrecognized OperatorType: %s", input));
    }
}

int operatorPrecedence(OperatorType t)
{
    switch (t)
    {
        case OperatorType.DotAccessor:
            return 19;
        case OperatorType.Asterisk:
        case OperatorType.Divide:
        case OperatorType.Modulo:
            return 14;
        case OperatorType.Plus:
        case OperatorType.Minus:
            return 13;
        case OperatorType.Equality:
        case OperatorType.Inequality:
            return 10;
        case OperatorType.LogicalAnd:
            return 6;
        default:
            throw new Exception(format("Unknown precedence for %s", t));
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

    bool rewind(int distance)
    {
        if (distance == 0 && distance > this.index)
        {
            return false;
        }

        this.index -= distance;
        return true;
    }

    Token peek(int distance)
    {
        int target = this.index + distance;

        if (target >= this.tokens.length)
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

    return ret;
}

Import parseImport(TokenFeed tokens)
{
    if (!tokens.current().match(TokenType.Word, "import"))
    {
        return null;
    }

    if (!tokens.next() || tokens.current().type != TokenType.Word)
    {
        throw new Exception("Expected a module name to import");
    }

    auto name = tokens.current().value;
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
        throw new Exception(format(
                "Couldn't find a file to match the imported module %s. " ~
                "Checked: %s", name, possibilities));
    }

    // Parse the import
    auto parsed = parse(name, lex(readText(filename)));

    FunctionSignature[] signatures;
    foreach (func; parsed.functions)
    {
        signatures ~= func.signature;
    }

    return new Import(filename, name, signatures);
}

Module parse(string name, Token[] tokenArray)
{
    auto tokens = new TokenFeed(tokenArray);

    Import[] imports;
    Function[] functions;
    FunctionSignature[] externs;
    Struct[] structs;
    Global[] globals;

    while (tokens.next())
    {
        auto imp = parseImport(tokens);
        if (imp !is null)
        {
            imports ~= imp;
            continue;
        }

        auto ext = parseExtern(tokens);
        if (ext !is null)
        {
            externs ~= ext;
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
            functions ~= method;
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

        throw new Exception("Unexpected token");
    }

    auto ret = new Module(name, imports, structs, functions, globals, externs);

    foreach (func; functions)
    {
        func.mod = ret;
    }

    validate(ret);

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

Struct parseStruct(TokenFeed tokens)
{
    if (!tokens.current().match(TokenType.Word, "struct"))
    {
        return null;
    }

    if (!tokens.next() || tokens.current().type != TokenType.Word)
    {
        throw new Exception("Expected a struct name");
    }

    auto name = tokens.current().value;

    if (!tokens.next() || !tokens.current().match(TokenType.Symbol, "{"))
    {
        throw new Exception("Expected a { after struct");
    }

    // Parse struct members
    TypeSignature[] members;
    while (tokens.next() && !tokens.current().match(TokenType.Symbol, "}"))
    {
        members ~= parseTypeSignature(tokens);

        // Assignment operator (=) or semi-colon
        if (!tokens.next() || !tokens.current().match(TokenType.Symbol, ";"))
        {
            throw new Exception("Expected semi-colon");
        }
    }

    return new Struct(name, members);
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

    auto sig = parseFunctionSignature(tokens);

    if (!tokens.next() || !tokens.current().match(TokenType.Symbol, ";"))
    {
        throw new Exception("Expected a semi-colon after extern");
    }

    return sig;
}

FunctionSignature parseFunctionSignature(TokenFeed tokens)
{
    // Return value
    auto token = tokens.current();

    if (token.type != TokenType.Word)
    {
        return null;
    }

    auto type = parseType(tokens);

    // Function name
    if (!tokens.next() || tokens.current().type != TokenType.Word)
    {
        return null;
    }

    auto name = tokens.current().value;

    TypeSignature[] params;
    bool variadic;
    if (!parseParameterList(tokens, &params, &variadic))
    {
        return null;
    }

    return new FunctionSignature(type, name, params, variadic);
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
    if (!tokens.next())
    {
        return null;
    }

    auto containerType = parseType(tokens);

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

    auto sig = parseFunctionSignature(tokens);
    if (sig is null)
    {
        tokens.index = rewindTarget;
        return null;
    }

    auto functionBody = parseBlock(tokens);

    return new Function(sig, functionBody);
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
        writeln(token);
        throw new Exception("Expected a function body");
    }

    Statement[] statements;

    while (tokens.next())
    {
        auto current = tokens.current();

        // End of function body
        if (current.type == TokenType.Symbol && current.value == "}")
        {
            break;
        }

        statements ~= parseStatement(tokens);
    }

    return new Block(statements);
}

Statement parseStatement(TokenFeed tokens)
{
    auto current = tokens.current();

    // Start of block
    if (current.match(TokenType.Symbol, "{"))
    {
        if (!tokens.rewind(1))
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
            default:
                auto localDeclaration = parseLocalDeclaration(tokens);
                if (localDeclaration !is null)
                {
                    return localDeclaration;
                }
        }
    }

    return parseAssignment(tokens);
}

// Try to parse a local declaration (like "u64 x = 3;") and return null if it
// can't be done.
LocalDeclaration parseLocalDeclaration(TokenFeed tokens)
{
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
    if (current.match(TokenType.Symbol, "="))
    {
        expression = new ExpressionParser(tokens).run();
    }
    else if (!current.match(TokenType.Symbol, ";"))
    {
        throw new Exception("Expected assignment operator (=) or semi-colon");
    }

    return new LocalDeclaration(null, typeSignature, expression);
}

Type parseType(TokenFeed tokens)
{
    auto name = tokens.current().value;

    bool pointer = false;
    auto next = tokens.peek(1);
    if (next.match(TokenType.Symbol, "*"))
    {
        pointer = true;
        tokens.next();
    }

    Node elements = null;
    next = tokens.peek(1);
    if (next.match(TokenType.Symbol, "["))
    {
        tokens.next();

        auto parser = new ExpressionParser(tokens);
        parser.until ~= new Token(TokenType.Symbol, "]");
        elements = parser.run();

        pointer = true;
    }

    return new IncompleteType(name, pointer=pointer, elements=elements);
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

Assignment parseAssignment(TokenFeed tokens)
{
    // parseExpression always starts by calling .next()
    tokens.rewind(1);
    auto lvalue = new ExpressionParser(tokens).run();

    auto current = tokens.current();

    if (current.type != TokenType.Symbol && current.value != "=")
    {
        throw new Exception("Expected assignment operator (=)");
    }

    // Expression (rvalue)
    auto expression = new ExpressionParser(tokens).run();

    return new Assignment(null, lvalue, expression);
}

Return parseReturn(TokenFeed tokens)
{
    return new Return(null, new ExpressionParser(tokens).run());
}

While parseWhile(TokenFeed tokens)
{
    auto block = parseConditionalBlock(tokens);

    return new While(block.conditional, block.block);
}

If parseIf(TokenFeed tokens)
{
    auto ifBlock = parseConditionalBlock(tokens);
    ConditionalBlock[] elseIfBlocks;
    Statement elseBlock = null;

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
        elseBlock = parseStatement(tokens);
        break;
    }

    return new If(ifBlock, elseIfBlocks, elseBlock);
}

// Parse a conditional expression followed by a statement or block
ConditionalBlock parseConditionalBlock(TokenFeed tokens)
{
    // Make sure there's an open parenthesis
    auto next = tokens.peek(1);

    if (next is null || !next.match(TokenType.Symbol, "("))
    {
        throw new Exception("Expected an if conditional");
    }

    auto conditional = parseExpressionParenthesis(tokens);
    auto block = parseStatement(tokens);

    return new ConditionalBlock(conditional, block);
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

        this.until ~= new Token(TokenType.Symbol, ";");
        this.until ~= new Token(TokenType.Symbol, "=");
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
        if (this.operators.len() == 0)
        {
            return;
        }

        if (this.operators.len() < 1 || this.output.len() < 2)
        {
            throw new Exception("Not enough on the stack(s) to consume!");
        }

        auto rightItem = this.output.pop();
        auto operator = parseOperatorType(this.operators.pop().token.value);
        auto leftItem = this.output.pop();

        auto right = getOrParseNode(rightItem);
        auto left = getOrParseNode(leftItem);

        Node op = null;
        if (operator == OperatorType.DotAccessor)
        {
            // Check for a member call
            auto call = cast(Call)rightItem.node;
            if (call !is null)
            {
                op = new MethodCall(left, call.functionName, call.parameters);
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

        this.outputPush(new ParserItem(op));
    }

    // Function calls can start like "func(" or "mod::func("
    bool startFunctionCall()
    {
        auto token0 = this.input.current();
        auto token1 = this.input.peek(1);
        auto token2 = this.input.peek(2);
        auto token3 = this.input.peek(3);

        if (token0.type != TokenType.Word)
        {
            return false;
        }

        if (token1.match(TokenType.Symbol, "("))
        {
            auto nameToken = new ParserItem(token0);
            nameToken.functionName = true;
            this.outputPush(nameToken);

            auto startList = new ParserItem(token1);
            startList.parameterListStart = true;
            this.operators.push(startList);

            this.input.next();

            return true;
        }

        if (token1.match(TokenType.Symbol, "::") &&
            token2.type == TokenType.Word &&
            token3.match(TokenType.Symbol, "("))
        {
            auto nameToken = new ParserItem(token2);
            nameToken.functionName = true;
            nameToken.moduleName = token0.value;
            this.outputPush(nameToken);

            auto startList = new ParserItem(token3);
            startList.parameterListStart = true;
            this.operators.push(startList);

            this.input.next();
            this.input.next();
            this.input.next();

            return true;
        }

        return false;
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
                            topOutput.token.value, parameters);

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
            int cur = operatorPrecedence(parseOperatorType(current.value));
            int prev = operatorPrecedence(parseOperatorType(
                    this.operators.peek(0).token.value));

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
        if (!this.input.next())
        {
            throw new Exception(
                    "Ran out of tokens while parsing an expression");
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

        // Detect dereference: word preceded by asterisk preceded by another
        // symbol or nothing
        if (current.match(TokenType.Symbol, "*") &&
            this.input.tokens.length > 0)
        {
            bool firstOperator = this.operators.stack.length == 0;

            bool precededBySymbol = false;
            if (this.operators.stack.length > 0)
            {
                auto topOperator = this.operators.peek(0);

                precededBySymbol = topOperator.isToken() &&
                                   topOperator.token.type == TokenType.Symbol;
            }

            if (firstOperator || precededBySymbol)
            {
                auto next = this.input.peek(1);

                if (next.type == TokenType.Word)
                {
                    this.output.push(new ParserItem(new Dereference(
                            parseToken(next))));

                    this.input.next();
                    return true;
                }
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
        while (this.next())
        {
        }

        if (this.output.len() != 1)
        {
            throw new Exception("Expected one node to be left in parser");
        }

        return getOrParseNode(this.output.pop());
    }
}

// Parse an expression in parenthesis, ending on the first close parenthesis
// with no open parenthesis on the stack
Node parseExpressionParenthesis(TokenFeed tokens)
{
    auto parser = new ExpressionParser(tokens);

    while (parser.next())
    {
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
                throw new Exception("Expected one node to be left in parser");
            }

            return getOrParseNode(parser.output.pop());
        }
    }

    throw new Exception("Expected a close parenthesis");
}

// Validation pass ------------------------------------------------------------

void completeFunction(Module mod, FunctionSignature func)
{
    func.returnType = completeType(mod, func.returnType);

    foreach (param; func.parameters)
    {
        param.type = completeType(mod, param.type);
    }

    // Inject 'this' parameter
    auto method = cast(MethodSignature)func;
    if (method !is null)
    {
        auto thisType = method.containerType.clone();
        thisType.pointer = true;
        auto thisParam = new TypeSignature(thisType, "this");
        func.parameters = thisParam ~ func.parameters;
    }
}

void validate(Module mod)
{
    // Complete struct definitions
    foreach (struct_; mod.structs)
    {
        foreach (member; struct_.members)
        {
            member.type = completeType(mod, member.type);
        }
    }

    // Complete globals
    foreach (global; mod.globals)
    {
        global.signature.type = completeType(mod, global.signature.type);
    }

    // Complete method container types
    foreach (method; mod.justMethods())
    {
        method.methodSignature.containerType = completeType(mod,
                method.methodSignature.containerType);
    }

    // Complete function signatures
    foreach (func; mod.functions)
    {
        completeFunction(mod, func.signature);
    }

    // Complete function bodies
    foreach (func; mod.functions)
    {
        validateStatement(mod, func.block);
    }
}

void validateStatement(Module mod, Statement st)
{
    foreach (childNode; st.childNodes)
    {
        validateNode(mod, childNode);
    }

    auto local = cast(LocalDeclaration)st;
    if (local !is null)
    {
        local.signature.type = completeType(mod, local.signature.type);

        if (local.signature.type.isPrimitive(Primitive.Auto))
        {
            if (local.value is null)
            {
                throw new Exception(format("Can't infer type for %s",
                        local.signature));
            }

            local.signature.type = local.value.type;
        }
    }

    foreach (childStatement; st.childStatements)
    {
        validateStatement(mod, childStatement);
    }
}

Type completeType(Module mod, Type type)
{
    if (type is null)
    {
        return null;
    }

    // Already completed, do nothing
    auto incomplete = cast(IncompleteType)type;
    if (incomplete is null)
    {
        return type;
    }

    // Complete the type
    Type ret = null;

    // Try primitives
    if (ret is null)
    {
        try
        {
            ret = new PrimitiveType(parsePrimitive(incomplete.name));
        }
        catch (Throwable)
        {
        }
    }

    // Try structs
    if (ret is null)
    {
        foreach (struct_; mod.structs)
        {
            if (struct_.name == incomplete.name)
            {
                ret = new StructType(struct_);
            }
        }
    }

    if (ret is null)
    {
        throw new Exception(format("Can't complete type %s", type));
    }

    ret.pointer = incomplete.pointer;
    ret.elements = incomplete.elements;

    return ret;
}

void validateNode(Module mod, Node node)
{
    foreach (childNode; node.childNodes())
    {
        validateNode(mod, childNode);
    }

    auto methodCall = cast(MethodCall)node;
    if (methodCall !is null)
    {
        methodCall.methodSignature = mod.findMethod(methodCall);
        node.retype();

        // Inject 'this' argument
        auto thisArg = new Reference(methodCall.container);
        thisArg.retype();
        methodCall.parameters = thisArg ~ methodCall.parameters;

        return;
    }

    auto call = cast(Call)node;
    if (call !is null)
    {
        call.targetSignature = mod.findFunction(call);
        node.retype();
        return;
    }

    auto binding = cast(Binding)node;
    if (binding !is null)
    {
        binding.local = findLocal(mod, binding, binding.name);
        node.retype();
        return;
    }

    auto sizeof = cast(SizeOf)node;
    if (sizeof !is null)
    {
        sizeof.argument = completeType(mod, sizeof.argument);
        node.retype();
        return;
    }

    node.type = completeType(mod, node.type);
    node.retype();
}

TypeSignature findLocal(Module mod, Node node, string name)
{
    // Walk up the tree until the first statement
    while (node.parent !is null)
    {
        auto parentNode = cast(Node)node.parent;
        if (parentNode !is null)
        {
            node = parentNode;
            continue;
        }

        auto statement = cast(Statement)node.parent;
        if (statement !is null)
        {
            return findLocal(mod, statement, name);
        }
    }

    throw new Exception(format("Found an orphaned node: %s", node));
}

Statement nextWithBlockParent(Statement st)
{
    while (cast(Block)st.parent is null)
    {
        auto parentStatement = cast(Statement)st.parent;
        if (parentStatement !is null)
        {
            st = parentStatement;
            continue;
        }

        return null;
    }

    return st;
}

TypeSignature findLocal(Module mod, Statement statement, string name)
{
    // Find the nearest Block ancestor
    statement = nextWithBlockParent(statement);
    if (statement is null)
    {
        throw new Exception("No more parents");
    }

    auto parent = cast(Block)statement.parent;

    // Check sibling statements that come before this statement in the block
    // for locals
    foreach (st; parent.statements)
    {
        if (st == statement)
        {
            continue;
        }

        auto local = cast(LocalDeclaration)st;
        if (local !is null)
        {
            if (local.signature.name == name)
            {
                return local.signature;
            }
        }
    }

    // Local not found in this block: continue walking up the tree
    auto parentStatement = cast(Statement)parent.parent;
    if (parentStatement !is null)
    {
        return findLocal(mod, parentStatement, name);
    }

    // Local not found in this function: check the function/method parameters
    auto func = cast(Function)parent.parent;
    if (func !is null)
    {
        auto parameters = func.signature.parameters;
        foreach (param; parameters)
        {
            if (param.name == name)
            {
                return param;
            }
        }
    }

    // Last resort: check the globals
    foreach (global; mod.globals)
    {
        if (global.signature.name == name)
        {
            return global.signature;
        }
    }

    throw new Exception(format("Local %s not in scope", name));
}
