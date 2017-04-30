import std.format;
import std.stdio;
import std.conv;
import std.ascii;

enum TokenType
{
    Integer,
    Word,
    Symbol,
    DoubleQuote,
}

class Token
{
    TokenType type;
    string value;

    pure this(TokenType type, string value)
    {
        this.type = type;
        this.value = value;
    }

    pure override string toString()
    {
        return format("%s\t%s", this.type, this.value);
    }

    pure bool match(TokenType type, string value)
    {
        return this.type == type && this.value == value;
    }

    pure bool match(Token other)
    {
        return this.match(other.type, other.value);
    }
}

immutable string[] symbols =
[
    "=",
    "+", "-", "*", "/",
    "!", "&",
    ";", ",",
    ".",
    "(", ")",
    "<", ">",
    "[", "]",
    "{", "}",

    "==", "!=", "&&", "::", ">=", "<="
];

pure bool isSymbol(string s)
{
    foreach (sym; symbols)
    {
        if (s == sym)
        {
            return true;
        }
    }

    return false;
}

pure bool isSymbol(dchar c)
{
    return isSymbol(to!string(c));
}

pure bool isWhiteSpace(dchar c)
{
    return c == ' ' || c == '\t' || c == '\n';
}

pure bool isDelimiter(dchar c)
{
    return isWhiteSpace(c) || isSymbol(c) || c == ':';
}

pure string resolveEscapeSequence(dchar second)
{
    switch (second)
    {
        case '\\':
            return "\\";
        case 'n':
            return "\n";
        case 't':
            return "\t";
        case '"':
            return "\"";
        default:
            throw new Exception(format(
                    "Unrecognized escape sequence: \\%s", second));
    }
}

// Represents one character in an input string at a time.
class Reader
{
    string input;
    int index = -1;

    pure this(string input)
    {
        this.input = input;
    }

    pure bool isFirst()
    {
        return this.index == 0;
    }

    pure bool isLast()
    {
        return this.index == this.input.length - 1;
    }

    pure void advance()
    {
        this.index++;
    }

    pure dchar current()
    {
        return this.input[this.index];
    }

    pure dchar next()
    {
        return this.input[this.index + 1];
    }

    pure Reader clone()
    {
        auto ret = new Reader(this.input);
        ret.index = this.index;
        return ret;
    }
}

Token[] lex(string src)
{
    auto reader = new Reader(src);
    Token[] tokens;

    while (true)
    {
        auto token = read(reader);

        if (token is null)
        {
            break;
        }

        tokens ~= token;
    }

    return tokens;
}

// Read the next token from the reader.
Token read(Reader r)
{
    if (r.isLast())
    {
        return null;
    }

    r.advance();

    skipWhiteSpace(r);

    if (r.isLast() && isWhiteSpace(r.current()))
    {
        return null;
    }

    Token ret;

    // Try to read a numeric token
    ret = tryRead(r, &readNumeric);
    if (ret !is null)
    {
        return ret;
    }

    // Try to read a symbol tokoen
    ret = tryRead(r, &readSymbol);
    if (ret !is null)
    {
        return ret;
    }

    // Try to read a double quote
    ret = tryRead(r, &readQuote);
    if (ret !is null)
    {
        return ret;
    }

    // Try to read a word token
    ret = tryRead(r, &readWord);
    if (ret !is null)
    {
        return ret;
    }

    throw new Exception(format(
            "Can't parse token starting with %c", r.current()));
}

// Run the function f and advance the reader only if f successfully read a
// token from the reader.
Token tryRead(Reader r, Token function(Reader) f)
{
    // Pass f a clone of the reader so if it fails to read a token it won't
    // mess up the reader in the process.
    auto clone = r.clone();
    auto ret = f(clone);

    if (ret is null)
    {
        return null;
    }

    // Token read: advance the reader to where the clone stopped reading.
    r.index = clone.index;
    return ret;
}

// Advance the reader until the next non-whitespace character.
void skipWhiteSpace(Reader r)
{
    while (!r.isLast() && isWhiteSpace(r.current()))
    {
        r.advance();
    }
}

Token readQuote(Reader r)
{
    if (r.current() != '"')
    {
        return null;
    }

    auto value = "";

    while (true)
    {
        r.advance();

        // Detect and deal with escape sequences
        if (r.current() == '\\')
        {
            r.advance();

            value ~= resolveEscapeSequence(r.current());

            continue;
        }

        // Detect and deal with double-escape quotes ("a""b")
        if (r.current() == '"' && !r.isLast() && r.next() == '"')
        {
            r.advance();

            value ~= "\"";

            continue;
        }

        // Detect end of quote
        if (r.current() == '"')
        {
            break;
        }

        value ~= r.current();
    }

    return new Token(TokenType.DoubleQuote, value);
}

Token readNumeric(Reader r)
{
    if (!isDigit(r.current()))
    {
        return null;
    }

    auto value = "";

    while (true)
    {
        value ~= r.current();

        if (r.isLast() || !isDigit(r.next()))
        {
            break;
        }

        r.advance();
    }

    return new Token(TokenType.Integer, value);
}

Token readSymbol(Reader r)
{
    string first = to!string(r.current());

    if (!r.isLast())
    {
        string both = first ~ to!string(r.next());

        if (isSymbol(both))
        {
            r.advance();
            return new Token(TokenType.Symbol, both);
        }
    }

    if (!isSymbol(first))
    {
        return null;
    }

    return new Token(TokenType.Symbol, first);
}

Token readWord(Reader r)
{
    if (isDelimiter(r.current()))
    {
        return null;
    }

    auto value = "";

    while (true)
    {
        value ~= r.current();

        if (r.isLast() || isDelimiter(r.next()))
        {
            break;
        }

        r.advance();
    }

    return new Token(TokenType.Word, value);
}
