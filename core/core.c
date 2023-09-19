#include <stdio.h>
#include <string.h>

typedef char bool;

void __rinha_print_str(const char *data, int len)
{
    if (data == NULL)
    {
        printf("<#null>\n");
        return;
    }
    printf("%.*s\n", len, data);
}

// deprecated: use fmt_int and print_str
void __rinha_print_int(int value) { printf("%d\n", value); }
// deprecated: use fmt_bool and print_str
void __rinha_print_bool(bool value) { printf("%s\n", value ? "true" : "false"); }

int __rinha_fmt_int(char *buf, int value)
{
    return sprintf(buf, "%d", value);
}
int __rinha_fmt_bool(char *buf, bool value)
{
    return sprintf(buf, "%s", value ? "true" : "false");
}
