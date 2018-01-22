import std.format;
import std.stdio;
import std.array;
import std.conv;

import globals;
import lexer;
import ast;
import grammar;

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
