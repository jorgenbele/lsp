#include <string.h>

#include "utils.h"

void *xmalloc(size_t n)
{
    void *ns = malloc(n);
    if (!ns) {
        fprintf(stderr, "xmalloc: Fatal, malloc failed!\n");
        exit(1);
    }
    return ns;
}

void *xcalloc(size_t nmembs, size_t n)
{
    void *ns = calloc(nmembs, n);
    if (!ns) {
        fprintf(stderr, "xcalloc: Fatal, calloc failed!\n");
        exit(1);
    }
    return ns;
}

char *xstrdupn(const char *s, size_t n)
{
    char *ns = calloc(1, n + 1);
    if (!ns) {
        fprintf(stderr, "xstrdupn: Fatal, calloc failed!\n");
        exit(1);
    }
    strncpy(ns, s, n);
    return ns;
}
