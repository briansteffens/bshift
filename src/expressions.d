import std.format;
import std.stdio;
import std.array;
import std.conv;
import std.variant;

import globals;
import debugging;
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
        case "reference":
            return OperatorType.Reference;
        case "dereference":
            return OperatorType.Dereference;
        default:
            throw new ParserException(format("Unrecognized operator: %s",
                    input));
    }
}

int precedence(OperatorType t)
{
    switch (t)
    {
        case OperatorType.DotAccessor:
            return 19;
        case OperatorType.Reference:
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
            throw new ParserException("Operator precedence unknown");
    }
}

int precedence(string o)
{
    return precedence(operatorType(o));
}

int operatorInputCount(OperatorType t)
{
    switch (t)
    {
        case OperatorType.Reference:
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
            throw new ParserException("OperatorType input count unknown");
    }
}

// This represents either an unparsed lexer Token or a parsed AST Node
alias ParserItem = Algebraic!(Token, Node);

class ParserItemStack
{
    ParserItem[] stack;

    void push(ParserItem i)
    {
        this.stack ~= i;
    }

    void push(Token t)
    {
        this.push(ParserItem(t));
    }

    void push(Node n)
    {
        this.push(ParserItem(n));
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
            if (t.value[0] == '-')
            {
                return new I64Literal(to!long(t.value));
            }
            else
            {
                return new U64Literal(to!ulong(t.value));
            }
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
    if (i.peek!(Node))
    {
        return i.get!(Node);
    }

    return parseToken(i.get!(Token));
}

class ExpressionParser
{
    TokenFeed input;

    Token start;
    Token previous;
    Token current;

    ParserItemStack output;
    ParserItemStack operators;

    // Set this to control when the parser stops.
    bool delegate(ExpressionParser) stopParsing;

    this(TokenFeed input)
    {
        this.input = input;
        this.start = input.current;

        this.output = new ParserItemStack();
        this.operators = new ParserItemStack();

        this.stopParsing = ep => ep.current.match(TokenType.Symbol, ";") ||
                                 ep.current.match(TokenType.Symbol, "=");
    }

    Node run()
    {
        current = input.current;

        if (verbose)
        {
            writeln("\nExpression parser starting: ", this.input);
        }

        printExpressionParserState(this);

        while (read())
        {
            if (!this.input.next())
            {
                throw new UnexpectedEOF(current);
            }

            current = input.current;

            printExpressionParserState(this);
        }

        if (output.len() != 1)
        {
            throw new SyntaxError("Invalid expression", current);
        }

        Node ret = getOrParseNode(output.pop());

        if (verbose)
        {
            writeln("Expression parser output: ", ret);
        }

        return ret;
    }

    bool read()
    {
        previous = this.input.peek(-1);
        current = this.input.current;

        hackOperatorTypes();

        if (this.checkForEnd())
        {
            return false;
        }

        // Consume as many operators as possible
        while (this.consumeOperatorPrecedence()) {}

        if (consumeSizeOf() || consumeCall() || consumeCast() ||
            consumeParenthesis() || consumeIndexer())
        {
            input.seek(-1); // TODO
            return true;
        }

        if (current.type != TokenType.Symbol)
        {
            this.output.push(ParserItem(current));
            return true;
        }

        hackStructMemberIndexing();

        // Operator was not handled: push it onto the operator stack.
        this.operators.push(current);
        return true;
    }

    // Detect reference and dereference operators. Resolve ambiguity between
    // operators with the same symbol.
    private void hackOperatorTypes()
    {
        if (!current.matchAnySymbol(["*", "&"]) || canCurrentBeInfix())
        {
            return;
        }

        switch (current.value)
        {
            case "*":
                current.value = "dereference";
                break;
            case "&":
                current.value = "reference";
                break;
            default:
                throw new Exception("Unrecognized operator " ~ current.value);
        }
    }

    // Analyze context near the current token to see if it can possibly be an
    // infix operator.
    private bool canCurrentBeInfix()
    {
        // TODO: Gotta be a better way to detect this. And this doesn't cover
        // all cases.
        return !previous.matchAny(TokenType.Word, ["return"]) &&
                !previous.matchAnySymbol(["(", "{", "[", ";", ",", "&&", "!=",
                "==", ">=", "<=", "<", ">", "=", "}"]);
    }

    // TODO
    private void hackStructMemberIndexing()
    {
        // Hack to allow indexing a struct member
        if (input.current.value != "[")
        {
            return;
        }

        while (this.operators.len() > 0)
        {
            auto topOperator = this.operators.peek(0);

            if (!topOperator.peek!(Token) ||
                !topOperator.get!(Token).match(TokenType.Symbol, "."))
            {
                break;
            }

            this.consumeOperator();
        }
    }

    // Get the number of open parenthesis on the operator stack
    int parenthesisDepth()
    {
        int ret = 0;

        foreach (item; this.operators.stack)
        {
            if (item.peek!(Token) &&
                item.get!(Token).match(TokenType.Symbol, "("))
            {
                ret++;
            }
        }

        return ret;
    }

    // Check for the end of the expression, returning true and consuming any
    // remaining operators if the end is found.
    private bool checkForEnd()
    {
        if (!stopParsing(this))
        {
            return false;
        }

        while (this.operators.len() > 0)
        {
            this.consumeOperator();
        }

        return true;
    }

    private bool consumeParenthesis()
    {
        return consumeBracket("(", ")");

        // TODO: Disambiguate between sub-expression, type-cast, and function
        // call.
    }

    bool consumeIndexer()
    {
        if (!consumeBracket("[", "]"))
        {
            return false;
        }

        // The sub-expression inside the brackets will be left on the top of
        // the output stack by consumeBracket().
        auto expression = getOrParseNode(output.pop());

        // TODO: Use generalized shunting yard rules to apply indexer to its
        // operand.
        auto operand = getOrParseNode(output.pop());

        output.push(new Indexer(operand, expression));

        return true;
    }

    private bool consumeBracket(string bracketStart, string bracketStop)
    {
        if (!current.match(TokenType.Symbol, bracketStop))
        {
            return false;
        }

        // TODO: empty

        while (!operators.peek(0).peek!(Token) ||
               !operators.peek(0).get!(Token).match(TokenType.Symbol,
                   bracketStart))
        {
            consumeOperator();
        }

        operators.pop();
        input.expectSymbol(bracketStop);
        return true;
    }

    // Consume the operator on top of the operator stack if the current input
    // operator has lower precedence than it.
    private bool consumeOperatorPrecedence()
    {
        if (current.type != TokenType.Symbol || operators.len() == 0)
        {
            return false;
        }

        try
        {
            int cur = precedence(current.value);
            int prev = precedence(operators.peek(0).get!(Token).value);

            if (prev < cur)
            {
                return false;
            }
        }
        catch (ParserException)
        {
            return false;
        }

        this.consumeOperator();
        return true;
    }

    // Consume the operator on top of the operator stack.
    private void consumeOperator()
    {
        auto operator = operatorType(operators.pop().get!(Token).value);
        Node node;

        switch (operatorInputCount(operator))
        {
            case 1:
                node = consumePrefixOperator(operator);
                break;
            case 2:
                node = consumeInfixOperator(operator);
                break;
            default:
                throw new Exception("Unrecognized inputCount");
        }

        output.push(ParserItem(node));
    }

    private Node consumeInfixOperator(OperatorType operator)
    {
        auto rightItem = this.output.pop();
        auto leftItem = this.output.pop();

        auto right = getOrParseNode(rightItem);
        auto left = getOrParseNode(leftItem);

        if (operator != OperatorType.DotAccessor)
        {
            return new Operator(left, operator, right);
        }

        // Check for a member call
        Call call;
        if (rightItem.peek!(Node))
        {
            call = cast(Call)rightItem.get!(Node);
        }

        if (call !is null)
        {
            return new MethodCall(left, call.functionName, call.parameters);
        }

        return new DotAccessor(left, rightItem.get!(Token).value);
    }

    private Node consumePrefixOperator(OperatorType operator)
    {
        auto rightItem = this.output.pop();
        auto right = getOrParseNode(rightItem);

        switch (operator)
        {
            case OperatorType.Reference:
                return new Reference(right);
            case OperatorType.Dereference:
                return new Dereference(right);
            default:
                throw new SyntaxError("Unrecognized operator", current);
        }
    }

    bool consumeGrammar(Node node)
    {
        if (node is null)
        {
            return false;
        }

        this.output.push(ParserItem(node));
        return true;
    }

    bool consumeCall()
    {
        // This will never support calling a function returned by an
        // expression. Not that you can do that now, but you probably should be
        // able to. And once you can, this will have to be handled in the
        // expression parser.
        return consumeGrammar(tryParse(input, &parseCall, null));
    }

    bool consumeSizeOf()
    {
        return consumeGrammar(tryParse(input, &parseSizeOf, null));
    }

    bool consumeCast()
    {
        return consumeGrammar(tryParse(input, &parseCast, null));
    }
}

Node parseExpression(TokenFeed tokens)
{
    return new ExpressionParser(tokens).run();
}

// Parse an expression in parenthesis, ending on the first close parenthesis
// with no open parenthesis on the stack
Node parseExpressionParenthesis(TokenFeed tokens)
{
    auto parser = new ExpressionParser(tokens);

    parser.stopParsing = ep =>
        ep.current.match(new Token(null, TokenType.Symbol, ")")) &&
        ep.parenthesisDepth() == 0;

    return parser.run();
}
