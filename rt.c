#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int __rinha_rt_print_str(char *data)
{
	int len = strlen(data);
	char *a = malloc(len + 1);
	strncpy(a, data, len);
	a[len] = 0;
	printf("%s\n", data);
}
