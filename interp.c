#include <stdio.h>
#include <assert.h>

#include "builtins.h"
#include "interp.h"
#include "utils.h"

DEF_VECTOR_FUNCS(interp_state, interp_state, ((interp_state) {NORMAL, 0}));
#if 0
lsp_list default_lsp_list = {
    .type = OBJ_LIST,
    .size = 0,
    .ptr = NULL,
    .vec = (vector_lsp_obj_ptr) {
        .size = 0, .len = 0, .error = 0, .allocs = 0, .data = NULL
    }
};
#endif
DEF_VECTOR_FUNCS(lsp_list_ptr, lsp_list_ptr, NULL);

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


// Create the abstract syntax tree from the provided
// tokens. The AST is itself stored as a lsp_lst,
// which means that the input (print 32 32 "123") will
// itself print to (print 32 32 "123").

// does not modify the contents in tokens,
// except that tokens->error may change.
// {SYMBOL PRINT, INT 24, FLOAT 32, STRING TEST} --> (print 24 32 TEST)
// Creates the abstract syntax tree for a given object
lsp_list *create_ast(vector_token *tokens)
{
    // The resulting ast is itelf stored in a
    // (recursive) list.
    lsp_list *ast = (lsp_list *) lsp_obj_new(OBJ_LIST);
    assert(ast);

    // Store a stack of lists. That way, T_LIST_END is
    // reached we know that the list at the top of the stack
    // has been closed, and it can be pushed to the list
    // below it. When there is nothing more to parse
    // the objects left in this stack is pushed to the
    // resulting list 'lst'.
    // NOTE allocated on the stack, but items are on the heap
    //      this makes handling of recursive lists easier.
    vector_lsp_list_ptr lst_stack;
    assert(!vector_init_lsp_list_ptr(&lst_stack));

    for (size_t i = 0; i < tokens->len; i++) {
        const token t = vector_get_token(tokens, i);
        assert(!tokens->error);

        switch (t.type) {
            case T_LIST_START: {
                // Push a new list to the lst_stack
                // NOTE allocated on the stack
                lsp_list *nlst = (lsp_list *) lsp_obj_new(OBJ_LIST);
                vector_push_lsp_list_ptr(&lst_stack, nlst);
                fprintf(stderr, "AST: starting list!\n");
                break;
            }

            case T_LIST_END: {
                // Push the list at the top of the stack
                // to the list of the one below it.
                lsp_list *top = vector_pop_lsp_list_ptr(&lst_stack);
                lsp_list *below = vector_peek_lsp_list_ptr(&lst_stack);
                if (!below) {
                    // This is the top.
                    fprintf(stderr,
                            "AST: reached end of list outside of list, "
                            "pushing to ast directly");
                    below = ast;
                }
                // Add the top list to the one below.
                vector_push_lsp_obj_ptr(&below->vec, (lsp_obj *) top);
                fprintf(stderr, "AST: pushing list!\n");
                lsp_obj_print_repr((lsp_obj *) top);
                break;
            }

            case T_SYMBOL: {
                break;
            }

            case T_STRING: {
                break;
            }

            case T_INT: {
                break;
            }

            case T_FLOAT: {
                break;
            }

            case T_BLANK:
            case T_NEWLINE:
            case T_CMT_START:
            case T_CMT_CONTENT:
            case T_CMT_END:
            case T_UNKNOWN:
                // skip
                break;
        }
    }

    // Copy elements from lst_stack to the ast.
    for (size_t i = 0; i < lst_stack.len && !lst_stack.error; i++) {
        lsp_list *lst = (vector_get_lsp_list_ptr(&lst_stack, i));
        assert(lst);
        vector_push_lsp_obj_ptr(&ast->vec, (lsp_obj *) lst);
    }
    // Destroy the lst_stack vector (not its contents)
    vector_destroy_lsp_list_ptr(&lst_stack);

    // TODO cleanup...
    return ast;
}



vector_lsp_obj_ptr *exec_tokens_(vector_token *tokens)
{
    // Stores a stack of the current parse state.
    // Is used for handling of recursive elements.
    vector_interp_state states;
    assert(!vector_init_interp_state(&states));

    // Stores the 'lsp_object's in a stack.
    vector_lsp_obj_ptr *stack = xcalloc(1, sizeof (*stack));
    assert(!vector_init_lsp_obj_ptr(stack));

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
                vector_push_interp_state(&states, (interp_state) {IN_LIST, stack->len});
                break;

            case T_LIST_END: {
                interp_state state = vector_pop_interp_state(&states);
                if (state.state != IN_LIST) {
                    // TODO cleanup
                    fprintf(stderr, "Runtime error: reached end of list while not inside a list\n!");
                    return NULL;
                }

                // Squash the stack into a lsp_list object.
                lsp_list lst;
                assert(!lsp_obj_init((lsp_obj *) &lst, OBJ_LIST));
                assert(!vector_init_lsp_obj_ptr(&lst.vec));
                size_t squashed = 0;
                for (size_t i = state.stack_start; i < stack->len; i++) {
                    lsp_obj *obj = vector_get_lsp_obj_ptr(stack, i);
                    assert(!stack->error);
                    vector_push_lsp_obj_ptr(&lst.vec, obj);
                    assert(!lst.vec.error);
                    squashed++;
                }

                // Pop the elements that was squashed
                // (they are still referenced in the squashed list)
                for (size_t i = 0; i < squashed; i++) {
                    lsp_obj *obj = vector_pop_lsp_obj_ptr(stack);
                    assert(!stack->error);
                    assert(obj);
                }

                const lsp_symbol *symb = (lsp_symbol *) vector_get_lsp_obj_ptr(&lst.vec, 0);
                if (stack->error || !symb) {
                    // No list element
                    stack->error = 0;
                } else if (symb->type == OBJ_SYMBOL) {
                    // Try to resolve the symbol
                    //fprintf(stderr, "symb: %s, `%s`, %lu\n", obj_type_str[symb->type], symb->symb, symb->symb_len);
                    BUILTIN_FUNC_PTR(func_ptr) = symb_get_func(symb->symb);
                    if (!func_ptr) {
                        fprintf(stderr, "Runtime error: `%s` is not a valid function!\n", symb->symb);
                        // TODO: below
                        // Not a function, push the value of the symbol
                    }
                    fprintf(stderr, "Runtime log: executing `%s`, argc:%lu!\n", symb->symb, lst.vec.len-1);

                    lsp_obj *ret = NULL;
                    if (lst.vec.len <= 1) {
                        ret = func_ptr(NULL);
                    } else {
                        ret = func_ptr(&lst.vec);
                    }
                    if (ret) {
                        vector_push_lsp_obj_ptr(stack, ret);
                    }
                } else {
                    // Is list but not function call

                    // Push to the stack
                    // Move to the heap.
                    lsp_list *lst_ptr = xcalloc(1, sizeof(*lst_ptr));
                    memcpy(lst_ptr, &lst, sizeof(lst));
                    vector_push_lsp_obj_ptr(stack, (lsp_obj_ptr) lst_ptr);
                    break; // EARLY
                }

                // Free the argument list
                while (true) {
                    lsp_obj *o = vector_pop_lsp_obj_ptr(&lst.vec);
                    if (lst.vec.error) {
                        break;
                    }
                    assert(o);
                    lsp_obj_destroy(o);
                    free(o);
                }
                vector_destroy_lsp_obj_ptr(&lst.vec);
                break;
            }

            case T_SYMBOL: {
                // Convert to symbol object and push to stack
                assert(t.is_str && t.str);
                lsp_symbol *symb = calloc(1, sizeof(*symb));
                assert(symb);
                assert(!lsp_obj_init((lsp_obj *)symb, OBJ_SYMBOL));
                symb->symb = xstrdupn(t.str, t.len);

                //fprintf(stderr, "PUSHING SYMBOL\n");
                vector_push_lsp_obj_ptr(stack, (lsp_obj *) symb);
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
                //fprintf(stderr, "PUSHING STRING\n");
                vector_push_lsp_obj_ptr(stack, (lsp_obj *) str);
                break;
            }

            case T_FLOAT: {
                // Convert to float object and push to stack
                assert(t.is_str && t.str);
                lsp_obj *flt = calloc(1, sizeof(*flt));
                assert(flt);
                assert(!lsp_obj_init((lsp_obj *)flt, OBJ_FLOAT));
                flt->flt = atof(t.str);
                vector_push_lsp_obj_ptr(stack, (lsp_obj *) flt);
                //fprintf(stderr, "PUSHING FLOAT\n");
                break;
            }

            case T_INT: {
                // Convert to int object and push to stack
                assert(t.is_str && t.str);
                lsp_obj *integer = xcalloc(1, sizeof(*integer));
                assert(integer);
                assert(!lsp_obj_init((lsp_obj *) integer, OBJ_INT));
                integer->integer = atoll(t.str);
                integer->type = OBJ_INT; //
                vector_push_lsp_obj_ptr(stack, (lsp_obj *) integer);
                //fprintf(stderr, "PUSHING INT\n");
                break;
            }

            case T_BLANK:
            case T_NEWLINE:
            case T_CMT_START:
            case T_CMT_CONTENT:
            case T_CMT_END:
            case T_UNKNOWN:
                // skip
                break;
        }
    }
    assert(!vector_destroy_interp_state(&states));
    return stack;
}

int exec_tokens(vector_token *tokens)
{
    vector_lsp_obj_ptr *stack = exec_tokens_(tokens);

    if (!stack) {
        return 1;
    }

    // TODO: free stack objects
    //fprintf(stderr, "STACK LEN: %lu\n", stack.len);
    while (!stack->error) {
        lsp_obj *obj = vector_pop_lsp_obj_ptr(stack);
        if (obj) {
            lsp_obj_print_repr(obj);
            lsp_obj_destroy(obj);
            free(obj);
        }
    }

    assert(!vector_destroy_lsp_obj_ptr(stack));
    return 0;
}
