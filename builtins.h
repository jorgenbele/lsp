#ifndef __BUILTINS_H_
#define __BUILTINS_H_

#include <stdlib.h>

#include "types.h"

#define BUILTIN_FUNC_PTR(name) lsp_obj_ptr (*name)(lsp_list *)

struct builtin {
    const char *symbol;
    BUILTIN_FUNC_PTR(func);
};
typedef struct builtin builtin;

extern const builtin builtins[];

void *builtin_get_func(const char *name);

lsp_obj *builtin_print(lsp_list *argl);
lsp_obj *builtin_println(lsp_list *argl);

lsp_obj *builtin_number_sum(lsp_list *argl);
lsp_obj *builtin_number_minus(lsp_list *argl);
lsp_obj *builtin_number_mult(lsp_list *argl);

lsp_obj *builtin_if(lsp_list *argl);
lsp_obj *builtin_quote(lsp_list *argl);
lsp_obj *builtin_repr(lsp_list *argl);
lsp_obj *builtin_eval(lsp_list *argl);
lsp_obj *builtin_repeat(lsp_list *argl);

#endif // __BUILTINS_H_
