string formatOutput(string s, string codes)
{
    return "\033[" ~ codes ~ "m" ~ s ~ "\033[0m";
}

string colorRed(string s)
{
    return formatOutput(s, "31;1");
}

string colorYellow(string s)
{
    return formatOutput(s, "33;1");
}

string colorGreen(string s)
{
    return formatOutput(s, "32;1");
}

string colorBlue(string s)
{
    return formatOutput(s, "34;1");
}
