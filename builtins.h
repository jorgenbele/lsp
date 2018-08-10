#ifndef __BUILTINS_H_
#define __BUILTINS_H_

#include <stdlib.h>

#include "types.h"

#define REQUIRES_N_ARGS(name, argl, n)                                  \
    if (lsp_list_len(argl) != n + 1) {                                  \
        fprintf(stderr, "Runtime error: failed to run `%s`, requires %d arguments!\n", name, n); \
        exit(1);                                                        \
    }

#define REQUIRES_ATLEAST_N_ARGS(name, argl, n)                          \
    if (lsp_list_len(argl) < n + 1) {                                   \
        fprintf(stderr, "Runtime error: failed to run `%s`, requires at least %d arguments!\n", name, n); \
        exit(1);                                                        \
    }

#define REQUIRES_MAXIMUM_N_ARGS(name, argl, n)                          \
    if (lsp_list_len(argl) > n + 1) {                                   \
        fprintf(stderr, "Runtime error: failed to run `%s`, takes maximum %d arguments!\n", name, n); \
        exit(1);                                                        \
    }


#define BUILTIN_FUNC_PTR(name) lsp_obj_ptr (*name)(lsp_list *)
typedef lsp_obj_ptr (*builtin_func_ptr)(lsp_list *);

struct builtin {
    const char *symbol;
    BUILTIN_FUNC_PTR(func);
};
typedef struct builtin builtin;

extern const builtin builtins[];

//void *builtin_get_func(const char *name);
builtin_func_ptr builtin_get_func(const char *name);

lsp_obj *builtin_progn(lsp_list *argl);
lsp_obj *builtin_print(lsp_list *argl);
lsp_obj *builtin_println(lsp_list *argl);

lsp_obj *builtin_int_not(lsp_list *argl);
lsp_obj *builtin_obj_eql(lsp_list *argl);
lsp_obj *builtin_obj_lt(lsp_list *argl);
lsp_obj *builtin_obj_gt(lsp_list *argl);
lsp_obj *builtin_obj_le(lsp_list *argl);
lsp_obj *builtin_obj_ge(lsp_list *argl);
lsp_obj *builtin_obj_cmp(lsp_list *argl);
lsp_obj *builtin_number_sum(lsp_list *argl);
lsp_obj *builtin_number_minus(lsp_list *argl);
lsp_obj *builtin_number_mult(lsp_list *argl);

lsp_obj *builtin_if(lsp_list *argl);
lsp_obj *builtin_quote(lsp_list *argl);
lsp_obj *builtin_repr(lsp_list *argl);
lsp_obj *builtin_eval(lsp_list *argl);
lsp_obj *builtin_repeat(lsp_list *argl);

lsp_obj *builtin_len(lsp_list *argl);
lsp_obj *builtin_car(lsp_list *argl);
lsp_obj *builtin_cdr(lsp_list *argl);
lsp_obj *builtin_append(lsp_list *argl);
lsp_obj *builtin_reverse(lsp_list *argl);

#endif // __BUILTINS_H_
