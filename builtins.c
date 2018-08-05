#include <stdio.h>
#include <assert.h>
#include <stdbool.h>

#include "builtins.h"
#include "utils.h"

const builtin builtins[] = {
    {"print", builtin_print},
    {"sum", builtin_number_sum},
    {NULL, NULL},
};

lsp_obj *builtin_print(vector_lsp_obj_ptr *argv)
{
    if (!argv) {
        return NULL;
    }

    // The first ptr in argv is the symbol which was used to call this
    // function. Skip it.
    for (size_t i = 1; i < argv->len; i++) {
        fprintf(stderr, "printing: %lu out of %lu\n", i, argv->len);
        const lsp_obj *ptr = vector_get_lsp_obj_ptr(argv, i);
        assert(!argv->error);
        assert(ptr);
        switch (ptr->type) {
            case OBJ_STRING:
                printf("%s", ptr->ptr);
                break;
            case OBJ_INT:
                printf("%ld", ptr->integer);
                break;
            case OBJ_FLOAT:
                printf("%lf", ptr->flt);
                break;

            case OBJ_SYMBOL: {
                lsp_symbol *symb = (lsp_symbol *) ptr;
                printf("#symbol:%s,%lu", symb->symb, symb->symb_len);
                break;
            }

            case OBJ_GENERIC: {
                printf("#generic:size:%lu", ptr->size);
                break;
            }
                
            case OBJ_LIST:
                putchar('(');
                // TODO:
                lsp_list *lst = (lsp_list *) ptr; // Create the memory to be passed to the function for (size_t i = 0; i < lst->vec.len; i++) {
                for (size_t k = 0; k < lst->vec.len; k++) {
                    vector_lsp_obj_ptr temp;
                    vector_init_w_lsp_obj_ptr(&temp, (lsp_obj *[]) {vector_get_lsp_obj_ptr(&lst->vec, k)}, 1);
                    if (k + 1 < lst->vec.len) {
                        putchar(',');  
                    } 
                    vector_destroy_lsp_obj_ptr(&temp);
                }
                putchar(')');
                break;
        }
        if (i + 1 < argv->len) {
            putchar(' ');
        }
    }
    return NULL;
}

lsp_obj *builtin_number_sum(vector_lsp_obj_ptr *argv)
{
    if (!argv) {
        return NULL;
    }

    bool using_float = false;
    int64_t int_sum = 0;
    double flt_sum = 0;

    for (size_t i = 1; i < argv->len; i++) {
        const lsp_obj *ptr = vector_get_lsp_obj_ptr(argv, i);
        assert(!argv->error);
        switch (ptr->type) {
            case OBJ_INT:
                int_sum += ptr->integer;
                break;

            case OBJ_FLOAT:
                using_float = true;
                flt_sum += ptr->flt;
                break;

            default:
                fprintf(stderr, "Runtime error: `sum` expected number!\n");
                exit(1);
                break;
        }
    }

    lsp_obj *res = xmalloc(sizeof(*res));
    if (using_float) {
        lsp_obj_init(res, OBJ_FLOAT);
        res->flt = flt_sum + int_sum;
    } else {
        lsp_obj_init(res, OBJ_INT);
        res->integer = int_sum;
    }
    return res;
}
