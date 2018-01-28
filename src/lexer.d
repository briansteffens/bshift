import std.format;
import std.stdio;
import std.conv;
import std.ascii;

import terminal;

class LexerError : Exception
{
    Char location;

    this(string message, Char location)
    {
        super(message);
        this.location = location;
    }
}

class Line
{
    int number;
    string source;
    string file;
    Line previous;
    Line next;

    this(int number, string file, Line previous, Line next)
    {
        this.number = number;
        this.file = file;
        this.previous = previous;
        this.next = next;
    }
}

class Char
{
    dchar value;
    Line line;
    int lineOffset;

    this(dchar value, Line line, int lineOffset)
    {
        this.value = value;
        this.line = line;
        this.lineOffset = lineOffset;
    }
}

enum TokenType
{
    Integer,
    Word,
    Symbol,
    SingleQuote,
    DoubleQuote,
}

class Token
{
    Char location;
    TokenType type;
    string value;

    pure this(Char location, TokenType type, string value)
    {
        this.location = location;
        this.type = type;
        this.value = value;
    }

    pure bool match(TokenType type, string value = null)
    {
        if (this.type != type)
        {
            return false;
        }

        if (value !is null)
        {
            return this.value == value;
        }

        return true;
    }

    pure bool match(Token other)
    {
        return this.match(other.type, other.value);
    }

    pure Token clone()
    {
        return new Token(this.location, this.type, this.value);
    }

    override string toString()
    {
        switch (this.type)
        {
            case TokenType.Integer:
                return colorRed(this.value);
            case TokenType.Word:
                return colorBlue(this.value);
            case TokenType.Symbol:
                return colorYellow(this.value);
            case TokenType.SingleQuote:
                return "'" ~ colorBlue(this.value) ~ "'";
            case TokenType.DoubleQuote:
                return "\"" ~ colorBlue(this.value) ~ "\"";
            default:
                throw new Exception("Unrecognized TokenType");
        }
    }
}

// Represents one character in an input string at a time.
class Reader
{
    Char[] input;
    string source;
    int index = -1;

    pure this(Char[] input, string source)
    {
        this.input = input;
        this.source = source;
    }

    pure bool isFirst()
    {
        return this.index == 0;
    }

    pure bool isLast()
    {
        return this.index == this.input.length - 1;
    }

    @property pure bool eof()
    {
        return this.index >= this.input.length;
    }

    void requireNotEOF()
    {
        if (eof)
        {
            //throw new UnexpectedEOF(
        }
    }

    pure void advance()
    {
        this.index++;
    }

    @property pure Char current()
    {
        return this.input[this.index];
    }

    pure dchar peek(int distance)
    {
        return this.input[this.index + distance].value;
    }

    pure dchar next()
    {
        return this.peek(1);
    }

    pure Reader clone()
    {
        auto ret = new Reader(this.input, this.source);

        ret.index = this.index;

        return ret;
    }
}

immutable string[] symbols =
[
    "=",
    "+", "-", "*", "/", "%",
    "!", "&",
    ";", ",",
    ".",
    "(", ")",
    "<", ">",
    "[", "]",
    "{", "}",

    "==", "!=", "&&", "||", "::", ">=", "<=",

    "<<", ">>",

    "...",
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
        case '\'':
            return "'";
        default:
            throw new Exception("We'll re-reraise this with more info");
    }
}

Char[] breakLines(string input, string file)
{
    Char[] ret;
    Line line = new Line(1, file, null, null);
    int lineOffset = 0;

    foreach (c; input)
    {
        if (c == '\n')
        {
            Line previous = line;
            line = new Line(previous.number + 1, file, previous, null);
            previous.next = line;
            lineOffset = 0;
        }
        else
        {
            line.source ~= c;
            lineOffset++;
        }

        ret ~= new Char(c, line, lineOffset);
    }

    return ret;
}

Token[] lex(string src, string srcName)
{
    auto reader = new Reader(breakLines(src, srcName), srcName);
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

alias readFunction = Token function(Reader);

immutable readFunction[] readFunctions = [
    &readNumeric,
    &readSymbol,
    &readDoubleQuote,
    &readSingleQuote,
    &readWord,
];

// Read the next token from the reader.
Token read(Reader r)
{
    if (r.isLast())
    {
        return null;
    }

    r.advance();

    // Skip non-tokens
    while (skipWhiteSpace(r) ||
           skipMultiLineComment(r) ||
           skipSingleLineComment(r))
    {
    }

    if (r.isLast() && isWhiteSpace(r.current.value))
    {
        return null;
    }

    // Try all the classes of token we know how to read
    foreach (readFunc; readFunctions)
    {
        auto ret = tryRead(r, readFunc);

        if (ret !is null)
        {
            return ret;
        }
    }

    throw new LexerError(format("Can't lex token %c", r.current.value),
            r.current);
}

// Run the function f and advance the reader only if f successfully read a
// token from the reader.
Token tryRead(Reader r, readFunction f)
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
bool skipWhiteSpace(Reader r)
{
    auto start = r.index;

    while (!r.isLast() && isWhiteSpace(r.current.value))
    {
        r.advance();
    }

    return start != r.index;
}

// If we're currently looking at a multi-line comment, advance the reader
// passed it.
bool skipMultiLineComment(Reader r)
{
    if (r.isLast() || r.current.value != '/' || r.next() != '*')
    {
        return false;
    }

    r.advance();

    while (r.current.value != '*' || r.next() != '/')
    {
        r.advance();
    }

    r.advance();
    r.advance();

    return true;
}

// If we're currently looking at a single-line comment, advance the reader
// passed it.
bool skipSingleLineComment(Reader r)
{
    if (r.isLast() || r.current.value != '/' || r.next() != '/')
    {
        return false;
    }

    r.advance();

    while (r.current.value != '\n')
    {
        r.advance();
    }

    r.advance();

    return true;
}

Token readDoubleQuote(Reader r)
{
    return readQuote(r, '"', TokenType.DoubleQuote);
}

Token readSingleQuote(Reader r)
{
    return readQuote(r, '\'', TokenType.SingleQuote);
}

Token readQuote(Reader r, char quote, TokenType type)
{
    if (r.current.value != quote)
    {
        return null;
    }

    auto start = r.current;
    auto value = "";

    while (true)
    {
        r.advance();

        // Detect and deal with escape sequences
        if (r.current.value == '\\')
        {
            r.advance();

            try
            {
                value ~= resolveEscapeSequence(r.current.value);
            }
            catch (Exception)
            {
                throw new LexerError(format("Unrecognized escape sequence %c",
                    r.current.value), r.current);
            }

            continue;
        }

        // Detect and deal with double-escape quotes ("a""b")
        if (r.current.value == quote && !r.isLast() && r.next() == quote)
        {
            r.advance();

            value ~= quote;

            continue;
        }

        // Detect end of quote
        if (r.current.value == quote)
        {
            break;
        }

        value ~= r.current.value;
    }

    return new Token(start, type, value);
}

Token readNumeric(Reader r)
{
    if (!isDigit(r.current.value))
    {
        return null;
    }

    auto start = r.current;
    auto value = "";

    while (true)
    {
        value ~= r.current.value;

        if (r.isLast() || !isDigit(r.next()))
        {
            break;
        }

        r.advance();
    }

    return new Token(start, TokenType.Integer, value);
}

Token readSymbol(Reader r)
{
    auto start = r.current;
    string[] possibilities;

    if (r.input.length - r.index >= 3)
    {
        possibilities ~= (to!string(r.current.value) ~
                          to!string(r.next()) ~
                          to!string(r.peek(2)));
    }

    if (!r.isLast())
    {
        possibilities ~= (to!string(r.current.value) ~
                          to!string(r.next()));
    }

    possibilities ~= to!string(r.current.value);

    foreach (possibility; possibilities)
    {
        if (isSymbol(possibility))
        {
            for (int i = 0; i < possibility.length - 1; i++)
            {
                r.advance();
            }

            return new Token(start, TokenType.Symbol, possibility);
        }
    }

    return null;
}

Token readWord(Reader r)
{
    if (isDelimiter(r.current.value))
    {
        return null;
    }

    auto start = r.current;
    auto value = "";

    while (true)
    {
        value ~= r.current.value;

        if (r.isLast() || isDelimiter(r.next()))
        {
            break;
        }

        r.advance();
    }

    return new Token(start, TokenType.Word, value);
}
