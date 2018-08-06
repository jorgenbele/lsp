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

    // Macro used to push to either the top lst_stack list,
    // or directly to ast.
#define PUSH_TOP_LIST_STACK_OR_AST(ptr)                             \
    do {                                                            \
        lsp_list *below = vector_peek_lsp_list_ptr(&lst_stack);     \
        if (!below) {                                               \
            fprintf(stderr,                                         \
                    "AST: reached end of list outside of list, "    \
                    "pushing to ast directly");                     \
            below = ast;                                            \
        }                                                           \
        vector_push_lsp_obj_ptr(&below->vec, (lsp_obj *) ptr);      \
        fprintf(stderr, "AST: pushing list!\n");                    \
        lsp_obj_print_repr((lsp_obj *) ptr);                        \
    } while (0);

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
                // Push either to the lst_stack OR directly to the AST,
                // see the PUSH_TOP_LIST_STACK_OR_AST marco.
                // This means that at the end the lst_stack will
                // empty as long as all lists are terminated.
                lsp_list *top = vector_pop_lsp_list_ptr(&lst_stack);
                PUSH_TOP_LIST_STACK_OR_AST(top);
                fprintf(stderr, "AST: pushing list!\n");
                lsp_obj_print_repr((lsp_obj *) top);
                break;
            }

            case T_SYMBOL: {
                assert(t.is_str);
                lsp_symbol *symb = (lsp_symbol *) lsp_obj_new(OBJ_SYMBOL);
                assert(symb);
                symb->symb = xstrdupn(t.str, t.len);
                symb->symb_len = t.len;
                PUSH_TOP_LIST_STACK_OR_AST(symb);
                break;
            }

            case T_STRING: {
                assert(t.is_str);
                lsp_str *lstr = (lsp_str *) lsp_obj_new_w(OBJ_STRING,
                                                          t.str, t.len);
                assert(lstr);
                PUSH_TOP_LIST_STACK_OR_AST(lstr);
                break;
            }

            case T_INT: {
                assert(t.is_str);
                lsp_obj *integer = lsp_obj_new(OBJ_INT);
                assert(integer);
                integer->integer = atoll(t.str);
                PUSH_TOP_LIST_STACK_OR_AST(integer);
                break;
            }

            case T_FLOAT: {
                assert(t.is_str);
                lsp_obj *flt = lsp_obj_new(OBJ_FLOAT);
                assert(flt);
                flt->flt = atof(t.str);
                PUSH_TOP_LIST_STACK_OR_AST(flt);
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

    if (lst_stack.len > 0) {
        // Destroy the lst_stack and the ast
        while (lst_stack.len > 0 && !lst_stack.error) {
            lsp_list *l = vector_pop_lsp_list_ptr(&lst_stack);
            lsp_obj_destroy((lsp_obj *) l);
            free(l);
        }
        vector_destroy_lsp_list_ptr(&lst_stack);
        lsp_obj_destroy((lsp_obj *) ast);
        free(ast);

        fprintf(stderr, "Runtime error: missing parenthesis?\n");
        return NULL;
    }

    // Destroy the lst_stack vector (not its contents)
    vector_destroy_lsp_list_ptr(&lst_stack);

    return ast;
}

lsp_list *execute_ast(lsp_list *ast)
{
    lsp_list *rlst = (lsp_list *) lsp_obj_new(OBJ_LIST);
    assert(rlst);

    for (size_t i = 0; i < ast->vec.len; i++) {
        // NOTE NOT CONST
        lsp_obj *obj = vector_get_lsp_obj_ptr(&ast->vec, i);
        assert(obj);

        switch (obj->type) {
            case OBJ_STRING:
            case OBJ_INT:
            case OBJ_FLOAT:
            case OBJ_SYMBOL:
            case OBJ_GENERIC: {
                // resolve to itslef
                lsp_obj *clone = lsp_obj_clone(obj);
                assert(clone);
                vector_push_lsp_obj_ptr(&rlst->vec, clone);
                break;
            }

            case OBJ_LIST: {
                // if the first element of the list is
                // a symbol, then execute the function call
                // on the symbol and push the result
                lsp_list *lst = (lsp_list *) obj;
                if (lst->vec.len > 0) {
                    lsp_obj *front = vector_get_lsp_obj_ptr(&lst->vec, 0);
                    assert(front);

                    if (front->type == OBJ_SYMBOL) {
                        // TODO actually to real symbol resolution
                        // to make sure that it is a function and
                        // not a variable/constant.
                        lsp_symbol *symb = (lsp_symbol *) front;
                        BUILTIN_FUNC_PTR(fptr) = builtin_get_func(symb->symb);
                        if (fptr) {
                            // executed
                            fprintf(stderr, "Runtime log: executing %s\n",
                                    symb->symb);
                            lsp_obj *robj = fptr(&lst->vec);
                            vector_push_lsp_obj_ptr(&rlst->vec, robj);
                            break;
                        }
                        fprintf(stderr, "Runtime error: unable to "
                                "resolve symbol: %s\n",
                                symb->symb);
                        exit(1);
                    }
                    // first element was not a symbol
                }
                // was not a function call, resolve to itself
                lsp_obj *clone = lsp_obj_clone(obj);
                assert(clone);
                vector_push_lsp_obj_ptr(&rlst->vec, clone);
                break;
            }
        }
    }
    return rlst;
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
                    BUILTIN_FUNC_PTR(func_ptr) = builtin_get_func(symb->symb);
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
