#include <stdio.h>
#include <assert.h>

#include "types.h"
#include "builtins.h"
#include "interp.h"
#include "utils.h"


static void *symb_get_func(const char *name)
{
    const builtin *ptr = builtins;
    while (ptr && ptr->symbol && ptr->func) {
        if (!strcmp(ptr->symbol, name)) {
            return ptr->func;
        }
        ptr++;
    }
    return  NULL;
}

int exec_tokens(vector_token *tokens)
{
    vector_lsp_obj_ptr stack;
    assert(!vector_init_lsp_obj_ptr(&stack));

    bool in_list = false;
    size_t list_start = (size_t) -1;

    for (size_t i = 0; i < tokens->len; i++) {
        const token t = vector_get_token(tokens, i);
        assert(!tokens->error);

        switch (t.type) {
            case T_LIST_START:
                // The lists contents are stored in the stack
                // like any other object during interpretation,
                // so the start position of the list has to be stored
                // in order to be able to retrieve it
                // (squash into list object).
                in_list = true;
                list_start = stack.len;

                //vector_push_lsp_obj_ptr(&stack, (){.})
                break;

            case T_LIST_END: {
                if (!in_list) {
                    // TODO cleanup
                    fprintf(stderr, "Runtime error: reached end of list while not inside a list\n!");
                    return 1;
                }

                // Squash the stack into a lsp_list object.
                lsp_list lst;
                assert(!vector_init_lsp_obj_ptr(&lst.vec));
                for (size_t i = list_start; i < stack.len; i++) {
                    lsp_obj *obj = vector_get_lsp_obj_ptr(&stack, i);
                    assert(!stack.error);
                    vector_push_lsp_obj_ptr(&lst.vec, obj);
                    assert(!lst.vec.error);
                }

                // NOTE: the stack data is now stored in lst.vec.data
                // which is an array.


                // If the first element in the list  is a
                // function, then the function should be
                // executed.
                // Check if the symbol is a function,
                // and execute it if so.
                const lsp_symbol *symb = (lsp_symbol *) vector_get_lsp_obj_ptr(&lst.vec, 0);
                if (stack.error) {
                    // no list element
                    stack.error = 0;
                } else {
                    fprintf(stderr, "symb: %s, `%s`, %lu\n", obj_type_str[symb->type], symb->symb, symb->symb_len);
                    assert(symb->type == OBJ_SYMBOL);
                    BUILTIN_FUNC_PTR(func_ptr) = symb_get_func(symb->symb);
                    if (!func_ptr) {
                        fprintf(stderr, "Runtime error: `%s` is not a valid function!\n", symb->symb);
                        // TODO: below
                        // Not a function, push the value of the symbol
                    }
                    fprintf(stderr, "Runtime log: executing `%s`, argc:%lu!\n", symb->symb, lst.vec.len-1);

                    if (lst.vec.len <= 1) {
                        vector_push_lsp_obj_ptr(&stack, func_ptr(NULL));
                    } else {
                        vector_push_lsp_obj_ptr(&stack, func_ptr(&lst.vec));
                    }
                }

                vector_destroy_lsp_obj_ptr(&lst.vec);

                //  reset
                in_list = false;
                list_start = (size_t) -1;
                break;
            }

            case T_UNKNOWN:
                break;

            case T_SYMBOL: {
                // Convert to symbol object and push to stack
                assert(t.is_str && t.str);
                lsp_symbol *symb = calloc(1, sizeof(*symb));
                assert(symb);
                assert(!lsp_obj_init((lsp_obj *)symb, OBJ_SYMBOL));
                symb->symb = xstrdupn(t.str, t.len);

                fprintf(stderr, "PUSHING SYMBOL\n");
                vector_push_lsp_obj_ptr(&stack, (lsp_obj *) symb);
                // TODO: lookup variables and replace with
                // the value of the variable/constant if it exists.
                break;
            }

            case T_STRING: {
                // Convert to string object and push to stack
                assert(t.is_str && t.str);
                lsp_str *str = calloc(1, sizeof(*str));
                assert(str);
                assert(!lsp_obj_init_w((lsp_obj *)str, OBJ_STRING, t.str, t.len));
                // TODO: lookup variables
                fprintf(stderr, "PUSHING STRING\n");
                vector_push_lsp_obj_ptr(&stack, (lsp_obj *) str);
                break;
            }

            case T_FLOAT: {
                // Convert to float object and push to stack
                assert(t.is_str && t.str);
                lsp_obj *flt = calloc(1, sizeof(*flt));
                assert(flt);
                assert(!lsp_obj_init((lsp_obj *)flt, OBJ_FLOAT));
                flt->flt = atof(t.str);
                vector_push_lsp_obj_ptr(&stack, (lsp_obj *) flt);
                break;
            }

            case T_INT: {
                // Convert to int object and push to stack
                assert(t.is_str && t.str);
                lsp_obj *integer = calloc(1, sizeof(*integer));
                assert(integer);
                assert(!lsp_obj_init((lsp_obj *) integer, OBJ_INT));
                integer->integer = atoll(t.str);
                vector_push_lsp_obj_ptr(&stack, (lsp_obj *) integer);
                break;
            }

            case T_BLANK:
            case T_NEWLINE:
            case T_CMT_START:
            case T_CMT_CONTENT:
            case T_CMT_END:
                // skip
                break;
        }
    }

    // TODO: free stack objects
    while (!stack.error) {
        lsp_obj *obj = vector_pop_lsp_obj_ptr(&stack);
        vector_lsp_obj_ptr temp;
        assert(!vector_init_w_lsp_obj_ptr(&temp, (lsp_obj *[]){NULL, obj}, 2));
        builtin_print(&temp);
        putchar('\n');

        vector_destroy_lsp_obj_ptr(&temp);
        if (obj) {
            lsp_obj_destroy(obj);
            free(obj);
        }
    }

    assert(!vector_destroy_lsp_obj_ptr(&stack));
    return 0;
}
