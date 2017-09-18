import std.format;
import std.stdio;
import std.conv;
import std.ascii;

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
    Line line;
    int lineOffset;
    TokenType type;
    string value;

    pure this(Line line, int lineOffset, TokenType type, string value)
    {
        this.line = line;
        this.lineOffset = lineOffset;
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

    pure Token clone()
    {
        return new Token(this.line, this.lineOffset, this.type, this.value);
    }
}

class ProgrammerException : Exception
{
    this(string msg, string item, Line line, int lineOffset,
         string efile = __FILE__, size_t eline = __LINE__)
    {
        auto fullMsg = format("[%s:%d,%d] %s \"%s\":\n%s",
                              line.file, line.number + 1, lineOffset,
                              msg, item, line.source);
        super(fullMsg, efile, eline);
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

class Line
{
    int number;
    string source;
    string file;

    this(int number, string file)
    {
        this.number = number;
        this.file = file;
    }
}

class Char
{
    dchar value;
    Line line;

    this(dchar value, Line line)
    {
        this.value = value;
        this.line = line;
    }
}

Char[] breakLines(string input, string file)
{
    Char[] ret;
    Line line = new Line(0, file);

    foreach (c; input)
    {
        if (c == '\n')
        {
            line = new Line(line.number + 1, file);
        }
        else
        {
            line.source ~= c;
        }

        ret ~= new Char(c, line);
    }

    return ret;
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

    pure void advance()
    {
        this.index++;
    }

    pure dchar current()
    {
        return this.input[this.index].value;
    }

    pure int currentLineOffset()
    {
        auto thisLine = this.currentLine();
        int offset = 0;
        foreach (c; this.input)
        {
            if (c.line.number == thisLine.number)
            {
                break;
            }
            offset++;
        }

        return this.index - offset;
    }

    pure Line currentLine()
    {
        return this.input[this.index].line;
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

    if (r.isLast() && isWhiteSpace(r.current()))
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

    auto line = r.currentLine();
    throw new ProgrammerException("Can't lex token", format("%c", r.current()),
                                  line, r.currentLineOffset());
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

    while (!r.isLast() && isWhiteSpace(r.current()))
    {
        r.advance();
    }

    return start != r.index;
}

// If we're currently looking at a multi-line comment, advance the reader
// passed it.
bool skipMultiLineComment(Reader r)
{
    if (r.isLast() || r.current() != '/' || r.next() != '*')
    {
        return false;
    }

    r.advance();

    while (r.current() != '*' || r.next() != '/')
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
    if (r.isLast() || r.current() != '/' || r.next() != '/')
    {
        return false;
    }

    r.advance();

    while (r.current() != '\n')
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
    if (r.current() != quote)
    {
        return null;
    }

    auto line = r.currentLine();
    auto value = "";

    while (true)
    {
        r.advance();

        // Detect and deal with escape sequences
        if (r.current() == '\\')
        {
            r.advance();

            try
            {
                value ~= resolveEscapeSequence(r.current());
            }
            catch (Exception)
            {
                throw new ProgrammerException(
                    "Unrecognized escape sequence",
                    format("%c", r.current()),
                    line,
                    r.currentLineOffset()
                );
            }

            continue;
        }

        // Detect and deal with double-escape quotes ("a""b")
        if (r.current() == quote && !r.isLast() && r.next() == quote)
        {
            r.advance();

            value ~= quote;

            continue;
        }

        // Detect end of quote
        if (r.current() == quote)
        {
            break;
        }

        value ~= r.current();
    }

    return new Token(line, r.currentLineOffset(), type, value);
}

Token readNumeric(Reader r)
{
    if (!isDigit(r.current()))
    {
        return null;
    }

    auto line = r.currentLine();
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

    return new Token(line, r.currentLineOffset(), TokenType.Integer, value);
}

Token readSymbol(Reader r)
{
    auto line = r.currentLine();
    string[] possibilities;

    if (r.input.length - r.index >= 3)
    {
        possibilities ~= (to!string(r.current()) ~
                          to!string(r.next()) ~
                          to!string(r.peek(2)));
    }

    if (!r.isLast())
    {
        possibilities ~= (to!string(r.current()) ~
                          to!string(r.next()));
    }

    possibilities ~= to!string(r.current());

    foreach (possibility; possibilities)
    {
        if (isSymbol(possibility))
        {
            for (int i = 0; i < possibility.length - 1; i++)
            {
                r.advance();
            }

            return new Token(line, r.currentLineOffset(), TokenType.Symbol, possibility);
        }
    }

    return null;
}

Token readWord(Reader r)
{
    if (isDelimiter(r.current()))
    {
        return null;
    }

    auto line = r.currentLine();
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

    return new Token(line, r.currentLineOffset(), TokenType.Word, value);
}
