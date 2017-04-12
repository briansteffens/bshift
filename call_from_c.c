#include <stdio.h>

unsigned long function_string_len(unsigned char* s);

void main()
{
    char* s = "greetings";
    printf("%d\n", function_string_len(s));
}
