#include <string.h>
#include <stdarg.h>
#include <unistd.h>

#include "utils.h"

void *xmalloc(size_t n)
{
    //void *ns = xcalloc(1, n); // ... dont use malloc
    void *ns = malloc(n); // ... dont use malloc
    if (!ns) {
        fprintf(stderr, "xmalloc: Fatal, malloc failed!\n");
        exit(1);
    }
    return ns;
}

void *xrealloc(void *s, size_t n)
{
    //fprintf(stderr, "** xrealloc %p **\n", s);
    void *ns = realloc(s, n);
    if (!ns) {
        fprintf(stderr, "xrealloc: Fatal, realloc failed!\n");
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
    char *ns = xcalloc(1, n + 1);
    if (!ns) {
        fprintf(stderr, "xstrdupn: Fatal, calloc failed!\n");
        exit(1);
    }
    strncpy(ns, s, n);
    return ns;
}

int alloc_sprintf(char **out, size_t *size, const char *fmt, ...)
{
    va_list va;
    va_start(va, fmt);
    ssize_t len = vsnprintf(NULL, 0, fmt, va);
    if (len < 0) {
        fprintf(stderr, "alloc_sprintf: Fatal, vsnprintf failed!\n");
        exit(1);
    }
    if (*size < ((size_t)len)+1) {
        *size = ((size_t)len)+1;
        *out = xrealloc(*out, *size);
    }
    va_end(va);
    va_start(va, fmt);
    if (vsnprintf(*out, *size, fmt, va) <= 0) {
        va_end(va);
        fprintf(stderr, "alloc_sprintf: Fatal, vsnprintf failed!\n");
        exit(1);
    }
    va_end(va);
    return 0;
}

int alloc_strcatf(char **out, size_t *size, const char *fmt, ...)
{
    va_list va;
    va_start(va, fmt);

    size_t out_len = 0;;
    if (*out) {
        out_len = strlen(*out);
    }

    ssize_t len = vsnprintf(NULL, 0, fmt, va);
    if (*size < out_len + len+1) {
        *size = out_len + len+1;
        *out = xrealloc(*out, *size);
    }
    va_end(va);
    va_start(va, fmt);

    char *end = *out + out_len;

    if (vsnprintf(end, *size, fmt, va) < 0) {
        // does not free.
        va_end(va);
        fprintf(stderr, "alloc_strcatf: Fatal, vsnprintf failed: %lu!\n", *size);
        exit(1);
    }
    va_end(va);
    return 0;
}
