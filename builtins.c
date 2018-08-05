#include <stdio.h>
#include <assert.h>

#include "builtins.h"

const builtin builtins[] = {
    {"print", builtin_print},
    {"print", builtin_print},
    {NULL, NULL},
};

int builtin_print(lsp_obj **objs, size_t len)
{
    if (!objs) {
        return 0;
    }

    for (size_t i = 0; i < len && objs[i]; i++) {
        switch (objs[i]->type) {
            case OBJ_STRING:
                printf("%s", objs[i]->ptr);
                break;
            case OBJ_INT:
                printf("%ld", objs[i]->integer);
                break;
            case OBJ_FLOAT:
                printf("%lf", objs[i]->flt);
                break;

            case OBJ_SYMBOL: {
                lsp_symbol *symb = (lsp_symbol *)(objs[i]);
                printf("#symbol:%s,%lu", symb->symb, symb->symb_len);
                break;
            }

            case OBJ_GENERIC: {
                printf("#generic:size:%lu", objs[i]->size);
                break;
            }
                
            case OBJ_LIST:
                putchar('(');
                // TODO:
                //lsp_list *lst = (lsp_list *) (*objs); // Create the memory to be passed to the function for (size_t i = 0; i < lst->vec.len; i++) {
                //    assert(!builtin_print(vector_get_lsp_obj_ptr(&lst, i)));
                //    if (i + 1 < lst->vec.len) {
                //        putchar(',');
                //    }
                //}
                putchar(')');
                break;
        }
        if (len) {
            putchar(' ');
        }
        objs++;
    }
    return 0;
}
