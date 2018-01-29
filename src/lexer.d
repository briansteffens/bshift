import std.format;
import std.stdio;
import std.conv;
import std.ascii;
import std.algorithm;

import terminal;

// Thrown when the lexer fails due to bad source code input
class LexerError : Exception
{
    // Approximate location in the source code where the error happened
    Char location;

    this(string message, Char location)
    {
        super(message);
        this.location = location;
    }
}

// Represents a line from a bshift source code file
class Line
{
    int number;
    string source;
    SourceFile file;
    Char[] chars;

    this(SourceFile file, int number)
    {
        this.file = file;
        this.number = number;
    }
}

// Represents a character from a bshift source code file
class Char
{
    dchar value;
    Line line;

    // The position of the character within the line
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
    // A numeric literal
    Integer,

    // Keywords, function names, variable names
    Word,

    // Operators, brackets, parenthesis
    Symbol,

    // A single quote: 'a'
    SingleQuote,

    // A double quote: "hello"
    DoubleQuote,

    // A comment. Not part of the token stream but still present in memory for
    // tagging Char instances.
    Comment,
}

class Token
{
    // The first character in the token
    Char location;
    TokenType type;
    string value;

    this(Char location, TokenType type, string value)
    {
        this.location = location;
        this.type = type;
        this.value = value;
    }

    bool match(TokenType type, string value = null)
    {
        return this.type == type && (value is null || this.value == value);
    }

    bool match(Token other)
    {
        return this.match(other.type, other.value);
    }

    Token clone()
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

// Represents a bshift source code file
class SourceFile
{
    string filename;
    Line[] lines;
    Char[] chars;

    this(string filename, string source)
    {
        this.filename = filename;
        this.lines = [];
        load(source);
    }

    // Convert source code into instances of Line and Char
    private void load(string source)
    {
        auto line = new Line(this, 1);
        int lineOffset = 0;

        foreach (c; source)
        {
            if (c == '\n')
            {
                lines ~= line;
                line = new Line(this, line.number + 1);
                lineOffset = 0;
            }
            else
            {
                line.source ~= c;
                lineOffset++;
            }

            auto chr = new Char(c, line, lineOffset);
            line.chars ~= chr;
            chars ~= chr;
        }

        if (lineOffset > 0)
        {
            lines ~= line;
        }
    }
}

// Represents a stream of source code in Char format. Allows peeking and
// seeking around the stream.
class Reader
{
    Char[] input;
    int index = 0;

    this(Char[] input)
    {
        this.input = input;
    }

    bool isLast()
    {
        return this.index == this.input.length - 1;
    }

    @property bool eof()
    {
        return this.index >= this.input.length;
    }

    // Throw an error if the reader is passed the end of the stream.
    private void requireNotEOF(int customIndex = -1)
    {
        customIndex = customIndex < 0 ? this.index : customIndex;

        if (customIndex < input.length)
        {
            return;
        }

        auto location = input[$-1];

        // Deal with case where there's no newline at the end of the file
        if (location.line.source == "")
        {
            location = input[$-2];
        }

        throw new LexerError("Unexpected end-of-file found", location);
    }

    void advance()
    {
        this.index++;
    }

    void seek(int delta)
    {
        index += delta;
        requireNotEOF();
    }

    Char peekChar(int distance)
    {
        auto target = this.index + distance;
        requireNotEOF(target);
        return this.input[target];
    }

    dchar peek(int distance)
    {
        return peekChar(distance).value;
    }

    @property Char currentChar()
    {
        return peekChar(0);
    }

    @property dchar current()
    {
        return currentChar.value;
    }

    string readUntil(bool delegate(Reader) predicate)
    {
        auto ret = to!string(current);

        while (!predicate(this))
        {
            advance();
            ret ~= current;
        }

        advance();

        return ret;
    }

    // Return true if the given array of dchars matches the current and
    // upcoming characters in the reader.
    bool matchSequence(dchar[] match)
    {
        if (index + match.length >= input.length)
        {
            return false;
        }

        for (int i = 0; i < match.length; i++)
        {
            if (peek(i) != match[i])
            {
                return false;
            }
        }

        return true;
    }

    // Return true if the given string matches the current and upcoming
    // characters in the reader.
    bool matchString(string match)
    {
        return matchSequence(to!(dchar[])(match));
    }

    // Advance the reader until it gets to 'end', returning the string read in
    // the process.
    string readUntilSequence(dchar[] end)
    {
        auto ret = readUntil(r => r.matchSequence(end));

        seek(cast(int)end.length - 1);

        return ret;
    }

    Reader clone()
    {
        auto ret = new Reader(this.input);

        ret.index = this.index;

        return ret;
    }

    // Create a new Token at the Reader's current location
    Token token(TokenType type, string value)
    {
        return new Token(currentChar, type, value);
    }
}

// Only these symbols are allowed by the lexer. These must be in descending
// order length-wise.
immutable string[] symbols =
[
    "...",

    "==", "!=", "&&", "||", "::", ">=", "<=",
    "<<", ">>",

    "=",
    "+", "-", "*", "/", "%",
    "!", "&",
    ";", ",",
    ".",
    "(", ")",
    "<", ">",
    "[", "]",
    "{", "}",
];

bool isSymbol(string s)
{
    return symbols.canFind(s);
}

bool isWhiteSpace(dchar c)
{
    return [' ', '\t', '\n'].canFind(c);
}

// Delimiters mark the end of a token
bool isDelimiter(dchar c)
{
    return isWhiteSpace(c) || isSymbol(to!string(c)) || c == ':';
}

string ensureTrailingNewline(string source)
{
    return source[$-1] == '\n' ? source : source ~ "\n";
}

// Convert source code into a list of tokens
Token[] lex(string filename, string source)
{
    source = ensureTrailingNewline(source);
    auto file = new SourceFile(filename, source);
    auto reader = new Reader(file.chars);

    Token[] tokens;
    Token token;

    while ((token = read(reader)) !is null)
    {
        if (token.type != TokenType.Comment)
        {
            tokens ~= token;
        }
    }

    return tokens;
}

// Advance the reader until the next non-whitespace character.
void skipWhiteSpace(Reader r)
{
    while (!r.eof && isWhiteSpace(r.current))
    {
        r.advance();
    }
}

// Read the next token from the reader.
Token read(Reader r)
{
    skipWhiteSpace(r);

    if (r.eof)
    {
        return null;
    }

    // Read the next token
    if (r.current == '\'')
    {
        return readSingleQuote(r);
    }
    else if (r.current == '"')
    {
        return readDoubleQuote(r);
    }
    else if (isDigit(r.current))
    {
        return readNumeric(r);
    }
    else if (r.matchSequence(['/', '*']))
    {
        return readMultiLineComment(r);
    }
    else if (r.matchSequence(['/', '/']))
    {
        return readSingleLineComment(r);
    }
    else if (isAlpha(r.current) || r.current == '_')
    {
        return readWord(r);
    }
    else
    {
        return readSymbol(r);
    }

    throw new LexerError(format("Can't lex token %c", r.current),
            r.currentChar);
}

Token readNumeric(Reader r)
{
    return r.token(TokenType.Integer, r.readUntil(r => !isDigit(r.peek(1))));
}

Token readWord(Reader r)
{
    return r.token(TokenType.Word, r.readUntil(r => isDelimiter(r.peek(1))));
}

Token readMultiLineComment(Reader r)
{
    return r.token(TokenType.Comment, r.readUntilSequence(['*', '/']));
}

Token readSingleLineComment(Reader r)
{
    return r.token(TokenType.Comment, r.readUntilSequence(['\n']));
}

Token readDoubleQuote(Reader r)
{
    return r.token(TokenType.DoubleQuote, readQuote(r, '"'));
}

Token readSingleQuote(Reader r)
{
    return r.token(TokenType.SingleQuote, readQuote(r, '\''));
}

Token readSymbol(Reader r)
{
    foreach (symbol; symbols)
    {
        if (r.matchSequence(to!(dchar[])(symbol)))
        {
            r.seek(cast(int)symbol.length);
            return new Token(r.currentChar, TokenType.Symbol, symbol);
        }
    }

    throw new LexerError("Unrecognized symbol", r.currentChar);
}

// Read an escape sequence from a string or throws LexerError
dchar readEscapeSequence(Reader r)
{
    r.seek(2);

    try
    {
        return expandEscapeSequence(r.peekChar(-1));
    }
    catch (Exception)
    {
        throw new LexerError(format("Unrecognized escape sequence %c",
            r.current), r.currentChar);
    }
}

// Decode an escape sequence, given the second character.
//   Example '\n': expandEscapeSequence('n') returns "\n"
dchar expandEscapeSequence(Char second)
{
    switch (second.value)
    {
        case '\\':
            return '\\';
        case 'n':
            return '\n';
        case 't':
            return '\t';
        case '"':
            return '"';
        case '\'':
            return '\'';
        default:
            throw new LexerError(format("Invalid escape sequence: \\%c",
                    second.value), second);
    }
}

// Read a character or escape sequence from a single or double quote
dchar readElementFromQuote(Reader r, dchar quote)
{
    // Slash escaped sequences: \n, \t
    if (r.current == '\\')
    {
        return readEscapeSequence(r);
    }

    // Quote escaped sequences: "", ''
    if (r.matchSequence([quote, quote]))
    {
        r.seek(2);
        return quote;
    }

    // Regular character: a, b
    r.advance();
    return r.peek(-1);
}

// Read a single or double quote
string readQuote(Reader r, dchar quote)
{
    auto value = "";

    r.advance();

    while (r.current != quote)
    {
        value ~= readElementFromQuote(r, quote);
    }

    r.advance();

    return value;
}
