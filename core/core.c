#include <stdio.h>
#include <string.h>
#include <stdlib.h>

typedef char bool;

int __rinha_print_str(const char *data, int len)
{
    if (data == NULL)
    {
        printf("<#null>\n");
        return 0;
    }
    printf("%.*s\n", len, data);
    return 0;
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

bool __rinha_memcmp(const void *a, const void *b, int bytes)
{
    int res = memcmp(a, b, bytes);
    return res == 0;
}

void __rinha_panic()
{
    printf("runtime error\n");
    exit(1);
}