#ifndef __BUILTINS_H_
#define __BUILTINS_H_

#include <stdlib.h>

#include "types.h"

#define BUILTIN_FUNC_PTR(name) int (*name)(lsp_obj **objs, size_t len)

struct builtin {
    const char *symbol;
    BUILTIN_FUNC_PTR(func);
};
typedef struct builtin builtin;

extern const builtin builtins[];

int builtin_print(lsp_obj **objs, size_t len);

#endif // __BUILTINS_H_
