import std.format;

enum TokenType
{
    Integer,
    Word,
    Symbol,
    DoubleQuote,
    SingleQuote,
}

bool isDigit(dchar c)
{
    return c == '0' || c == '1' || c == '2' || c == '3' || c == '4' ||
           c == '5' || c == '6' || c == '7' || c == '8' || c == '9';
}

bool isWhiteSpace(dchar c)
{
    return c == ' ' || c == '\t' || c == '\n' || c == '"' || c == '\'';
}

bool isSymbol(dchar c)
{
    return c == '+' || c == '=' || c == '(' || c == ')' || c == ';' ||
           c == '{' || c == '}';
}

bool isDelimiter(dchar c)
{
    return isWhiteSpace(c) || isSymbol(c);
}

TokenType identifyTokenType(string value, dchar quote)
{
    if (quote == '\'')
    {
        return TokenType.SingleQuote;
    }

    if (quote == '"')
    {
        return TokenType.DoubleQuote;
    }

    if (value.length == 1 && isSymbol(value[0]))
    {
        return TokenType.Symbol;
    }

    for (int i = 0; i < value.length; i++)
    {
        if (!isDigit(value[i]))
        {
            return TokenType.Word;
        }
    }

    return TokenType.Integer;
}

class Token
{
    TokenType type;
    string value;

    this(TokenType type, string value)
    {
        this.type = type;
        this.value = value;
    }

    override string toString()
    {
        return format("%s\t%s", this.type, this.value);
    }
}

Token[] lex(string src)
{
    if (src.length < 2)
    {
        throw new Exception("src must be at least 2 characters");
    }

    Token[] tokens;

    dchar prev;
    dchar current;
    dchar next = src[0];

    dchar quote = false;
    int quoteStart = 0;

    bool nextEscaped = false;
    bool currentEscaped = false;

    string building = "";
    dchar buildingQuote = false;

    for (int i = 0; i < src.length; i++)
    {
        bool isFirst = i == 0;
        bool isLast  = i == src.length - 1;

        prev = current;
        current = next;
        currentEscaped = nextEscaped;

        if (!isLast)
        {
            next = src[i + 1];
        }

        bool nextSlashEscaped = !isLast && current == '\\';
        bool nextDoubleEscaped = !isLast && next == quote &&
                                 current == quote && quoteStart < i;

        nextEscaped = !isLast && !currentEscaped &&
                      (nextSlashEscaped || nextDoubleEscaped);

        bool isCurrentQuote = !currentEscaped && !nextDoubleEscaped &&
                              (current == '\'' || current == '"');

        bool quoteToggledThisLoop = false;

        // Start of quote
        if (isCurrentQuote && quote == false)
        {
            quote = current;
            buildingQuote = current;
            quoteToggledThisLoop = true;
            quoteStart = i;
        }

        // End of quote
        if (isCurrentQuote && quote && !quoteToggledThisLoop &&
            quote == current)
        {
            quote = false;
        }

        if ((isLast || !quote && isDelimiter(current)) && building.length > 0)
        {
            tokens.length++;
            auto type = identifyTokenType(building, buildingQuote);
            tokens[tokens.length - 1] = new Token(type, building);
            building = "";
            buildingQuote = false;
        }
        else if (!quoteToggledThisLoop &&
                 (quote && !nextEscaped || !quote && !isWhiteSpace(current)))
        {
            building ~= current;
        }
    }

    return tokens;
}
