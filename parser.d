import std.stdio;
import std.format;
import std.conv;

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

    // Close parenthesis
    if (!tokens.next())
    {
        throw new Exception("Expected a close parenthesis");
    }

    token = tokens.current();

    if (token.type != TokenType.Symbol || token.value != ")")
    {
        throw new Exception("Expected a close parenthesis");
    }

    // Open bracket
    if (!tokens.next())
    {
        throw new Exception("Expected a function body");
    }

    token = tokens.current();

    if (token.type != TokenType.Symbol || token.value != "{")
    {
        throw new Exception("Expected a function body");
    }

    // Function body
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

    return new Function(type, name, statements);
}

Statement parseStatement(TokenFeed tokens)
{
    auto current = tokens.current();

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

    // Semi-colon
    if (!tokens.next())
    {
        throw new Exception("Expected semi-colon");
    }

    current = tokens.current();

    if (current.type != TokenType.Symbol || current.value != ";")
    {
        throw new Exception("Expected semi-colon");
    }

    return new LocalDeclaration(null, type, name);
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

    void push(Token t)
    {
        this.stack ~= new ParserItem(t);
    }

    void push(Node n)
    {
        this.stack ~= new ParserItem(n);
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

    void consume()
    {
        if (this.operators.len() < 1 || this.output.len() < 2)
        {
            throw new Exception("Not enough on the stack(s) to consume!");
        }

        auto right = getOrParseNode(this.output.pop());
        auto operator = parseOperatorType(this.operators.pop().token.value);
        auto left = getOrParseNode(this.output.pop());

        this.output.push(new Operator(left, operator, right));
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
            this.consume();
            return false;
        }

        if (current.type != TokenType.Symbol)
        {
            this.output.push(current);
            return true;
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
    writefln("----------------------------------------------");

    if (parser.output.len() != 1)
    {
        throw new Exception("Expected one node to be left in parser");
    }

    return getOrParseNode(parser.output.pop());
}
