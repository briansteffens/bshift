import std.stdio;
import std.conv;
import std.format;
import std.algorithm.comparison;

import terminal;
import lexer;

void debugTokens(Token[] tokens)
{
    writeln("bshift tokens -------------------------------------------\n");

    int indentSpaces(Line line)
    {
        int ret = 0;

        foreach (c; line.chars)
        {
            if (c.value == '\t')
            {
                // TODO
                ret += 4;
            }
            else if (c.value == ' ')
            {
                ret += 1;
            }
            else
            {
                break;
            }
        }

        return ret;
    }

    string indented(int level)
    {
        string ret = "";

        while (level-- > 0)
        {
            ret ~= " ";
        }

        return ret;
    }

    Line line = null;
    string buffer = "";

    foreach (token; tokens)
    {
        if (line is null)
        {
            line = token.location.line;
        }

        if (buffer != "")
        {
            buffer ~= " ";
        }

        buffer ~= token.toString();

        if (line != token.location.line)
        {
            if (line !is null)
            {
                writeln(indented(indentSpaces(line)) ~ buffer);
            }

            line = token.location.line;
            buffer = "";
        }
    }

    if (buffer != "")
    {
        writeln(indented(indentSpaces(line)) ~ buffer);
    }

    writeln();
}

void printSyntaxError(string message, Char location)
{
    string padRight(string source, ulong maxLength)
    {
        while (source.length < maxLength)
        {
            source ~= " ";
        }

        return source;
    }

    writeln("\n" ~ colorRed("error: ") ~ message);

    // Filename
    writeln(format(" %s %s:%d:%d", colorBlue("-->"),
            location.line.file.filename, location.line.number,
            location.lineOffset));

    // Source code
    auto middleLine = location.line;
    auto file = middleLine.file;

    int start = max(0, middleLine.number - 5);
    int stop = min(file.lines.length - 1, middleLine.number + 4);

    Line[] lines = file.lines[start..stop + 1];

    // Padding for line numbers
    auto padding = to!string(middleLine.number).length;

    // Output source code
    foreach (line; lines)
    {
        string left = "";

        if (line == middleLine)
        {
            left = to!string(line.number);
        }

        left = padRight(left, padding);

        writeln(format("%s | %s", left, line.source));

        if (line == middleLine)
        {
            writeln(padRight("", padding + 2 + location.lineOffset),
                    colorRed("^ somewhere around here"));
        }
    }

    writeln();
}
