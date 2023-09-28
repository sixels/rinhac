#include <stdio.h>
#include <string.h>
#include <stdlib.h>

typedef char bool;

struct string
{
    char *data;
    int len;
};

struct boxed
{
    char tag;
    char data[47];
};

struct tuple
{
    char first_tag;
    char first_data[23];
    char second_tag;
    char second_data[23];
};

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

int __rinha_fmt_boxed(char *buf, char tag, char data[23]);

int __rinha_fmt_tuple(char *buf, struct tuple *tuple)
{
    int len = 0;

    len += sprintf(buf, "(");
    len += __rinha_fmt_boxed(buf + len, tuple->first_tag, tuple->first_data);
    len += sprintf(buf + len, ",");
    len += __rinha_fmt_boxed(buf + len, tuple->second_tag, tuple->second_data);
    len += sprintf(buf + len, ")");

    return len;
}

int __rinha_fmt_boxed(char *buf, char tag, char data[23])
{
    switch (tag)
    {
    case 1 << 0:
        return __rinha_fmt_int(buf, *(int *)(data + 19));
    case 1 << 1:
        return __rinha_fmt_bool(buf, *(bool *)(data + 22));
    case 1 << 2:
        struct string str = *(struct string *)(data + 11);
        return sprintf(buf, "%.*s", str.len, str.data);
    case 1 << 3:
        return sprintf(buf, "<#closure>");
    case 1 << 4:
        return __rinha_fmt_tuple(buf, (struct tuple *)data);
    default:
        return 0;
    }
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