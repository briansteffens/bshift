import std.stdio;
import std.format;
import std.conv;
import std.array;

import lexer;
import ast;

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

Module parse(Token[] tokenArray)
{
    auto tokens = new TokenFeed(tokenArray);

    Function[] functions;

    while (tokens.next())
    {
        auto current = tokens.current();

        if (current.type != TokenType.Word)
        {
            throw new Exception("Unexpected token; expected function");
        }

        functions.length++;
        functions[functions.length - 1] = parseFunction(tokens);
    }

    return new Module(functions);
}

Function parseFunction(TokenFeed tokens)
{
    // Return value
    auto token = tokens.current();

    if (token.type != TokenType.Word)
    {
        throw new Exception("Expected a function return type");
    }

    auto type = parseType(token.value);

    // Function name
    if (!tokens.next())
    {
        throw new Exception("Expected a function name");
    }

    token = tokens.current();

    if (token.type != TokenType.Word)
    {
        throw new Exception("Expected a function name");
    }

    auto name = token.value;

    // Open parenthesis
    if (!tokens.next())
    {
        throw new Exception("Expected a function parameter list");
    }

    token = tokens.current();

    if (token.type != TokenType.Symbol || token.value != "(")
    {
        throw new Exception("Expected a function parameter list");
    }

    // Parameter list
    TypeSignature[] parameters;

    while (tokens.next())
    {
        token = tokens.current();

        // End of parameter list
        if (token.type == TokenType.Symbol && token.value == ")")
        {
            break;
        }

        parameters ~= parseTypeSignature(tokens);

        // Commas separate parameters
        auto next = tokens.peek(1);
        if (next !is null &&
            next.type == TokenType.Symbol &&
            next.value == ",")
        {
            tokens.next();
        }
    }

    auto functionBody = parseBlock(tokens);

    return new Function(type, name, parameters, functionBody);
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

    Statement[] statements;

    while (tokens.next())
    {
        auto current = tokens.current();

        // End of function body
        if (current.type == TokenType.Symbol && current.value == "}")
        {
            break;
        }

        statements.length++;
        statements[statements.length - 1] = parseStatement(tokens);
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
        if (current.value == "return")
        {
            return parseReturn(tokens);
        }

        try
        {
            parseType(current.value);
            return parseLocalDeclaration(tokens);
        }
        catch
        {
            // Not a local declaration. Bury exception.
        }
    }

    return parseAssignment(tokens);
}

LocalDeclaration parseLocalDeclaration(TokenFeed tokens)
{
    auto typeSignature = parseTypeSignature(tokens);

    // Semi-colon
    if (!tokens.next())
    {
        throw new Exception("Expected semi-colon");
    }

    auto current = tokens.current();

    if (current.type != TokenType.Symbol || current.value != ";")
    {
        throw new Exception("Expected semi-colon");
    }

    return new LocalDeclaration(null, typeSignature);
}

TypeSignature parseTypeSignature(TokenFeed tokens)
{
    // New local type
    auto type = parseType(tokens.current().value);

    // New local name
    if (!tokens.next())
    {
        throw new Exception("Expected new local name");
    }

    auto current = tokens.current();

    if (current.type != TokenType.Word)
    {
        throw new Exception("Expected new local name");
    }

    auto name = current.value;

    return new TypeSignature(type, name);
}

Assignment parseAssignment(TokenFeed tokens)
{
    // Binding (lvalue)
    auto current = tokens.current();

    if (current.type != TokenType.Word)
    {
        throw new Exception(format(
                "Expected a variable name to assign to. Got: %s", current));
    }

    auto binding = new Binding(current.value);

    // Assignment operator (=)
    if (!tokens.next())
    {
        throw new Exception("Expected assignment operator (=)");
    }

    current = tokens.current();

    if (current.type != TokenType.Symbol && current.value != "=")
    {
        throw new Exception("Expected assignment operator (=)");
    }

    // Expression (rvalue)
    auto expression = parseExpression(tokens);

    return new Assignment(null, binding, expression);
}

Return parseReturn(TokenFeed tokens)
{
    return new Return(null, parseExpression(tokens));
}

// This represents either an unparsed lexer Token or a parsed AST Node
class ParserItem
{
    Token token;
    Node node;

    // Represents the function name in a call
    bool functionName;

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
            return new ULongLiteral(to!ulong(t.value));
        case TokenType.Word:
            return new Binding(t.value);
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

    this(TokenFeed input)
    {
        this.input = input;

        this.output = new ParserItemStack();
        this.operators = new ParserItemStack();
    }

    void printState()
    {
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

        auto right = getOrParseNode(this.output.pop());
        auto operator = parseOperatorType(this.operators.pop().token.value);
        auto left = getOrParseNode(this.output.pop());

        this.outputPush(new ParserItem(new Operator(left, operator, right)));
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
                this.outputPush(new ParserItem(
                            new Call(topOutput.token.value, parameters)));
                break;
            }

            insertInPlace(parameters, 0, getOrParseNode(topOutput));
        }
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

        Type castType;
        try
        {
            castType = parseType(cur.value);
        }
        catch
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

    bool next()
    {
        if (!this.input.next())
        {
            throw new Exception(
                    "Ran out of tokens while parsing an expression");
        }

        this.current = this.input.current();
        this.printState();

        if (current.type == TokenType.Symbol && current.value == ";")
        {
            while (this.operators.len() > 0)
            {
                this.consume();
            }
            return false;
        }

        // Check for the beginning of a function call: word followed by (
        if (current.type == TokenType.Word)
        {
            auto nextInput = this.input.peek(1);

            if (nextInput !is null &&
                nextInput.type == TokenType.Symbol &&
                nextInput.value == "(")
            {
                auto nameToken = new ParserItem(current);
                nameToken.functionName = true;
                this.outputPush(nameToken);

                this.input.next();
                auto startList = new ParserItem(this.input.current());
                startList.parameterListStart = true;
                this.operators.push(startList);

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
}

Node parseExpression(TokenFeed tokens)
{
    auto parser = new ExpressionParser(tokens);
    while (parser.next())
    {
    }
    //writefln("----------------------------------------------");

    if (parser.output.len() != 1)
    {
        throw new Exception("Expected one node to be left in parser");
    }

    return getOrParseNode(parser.output.pop());
}
