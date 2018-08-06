#ifndef __BUILTINS_H_
#define __BUILTINS_H_

#include <stdlib.h>

#include "types.h"

#define BUILTIN_FUNC_PTR(name) lsp_obj_ptr (*name)(vector_lsp_obj_ptr *argv)

struct builtin {
    const char *symbol;
    BUILTIN_FUNC_PTR(func);
};
typedef struct builtin builtin;

extern const builtin builtins[];

void *builtin_get_func(const char *name);

//lsp_obj *builtin_repr(vector_lsp_obj_ptr *argv);
lsp_obj *builtin_print(vector_lsp_obj_ptr *argv);
lsp_obj *builtin_number_sum(vector_lsp_obj_ptr *argv);
lsp_obj *builtin_number_minus(vector_lsp_obj_ptr *argv);
lsp_obj *builtin_number_add(vector_lsp_obj_ptr *argv);
lsp_obj *builtin_if(vector_lsp_obj_ptr *argv);
lsp_obj *builtin_repr(vector_lsp_obj_ptr *argv);

#endif // __BUILTINS_H_
