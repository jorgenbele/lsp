#ifndef __UTILS_H_
#define __UTILS_H_

#include <stdlib.h>
#include <stdio.h>

#define parse_error printf

void *xmalloc(size_t n);
void *xrealloc(void *s, size_t n);
void *xcalloc(size_t nmembs, size_t n);
char *xstrdupn(const char *s, size_t n);
int alloc_sprintf(char **out, size_t *size, const char *fmt, ...);
int alloc_strcatf(char **out, size_t *size, const char *fmt, ...);

#endif // __UTILS_H_
