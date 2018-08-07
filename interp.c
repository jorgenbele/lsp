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
            below = ast;                                            \
        }                                                           \
        vector_push_lsp_obj_ptr(&below->vec, (lsp_obj *) ptr);      \
    } while (0);
            //fprintf(stderr,
            //        "AST: reached end of list outside of list, "
            //        "pushing to ast directly");
        //fprintf(stderr, "AST: pushing list!\n");
        //lsp_obj_print_repr((lsp_obj *) ptr);

    for (size_t i = 0; i < tokens->len; i++) {
        const token t = vector_get_token(tokens, i);
        assert(!tokens->error);

        switch (t.type) {
            case T_LIST_START: {
                // Push a new list to the lst_stack
                // NOTE allocated on the stack
                lsp_list *nlst = (lsp_list *) lsp_obj_new(OBJ_LIST);
                vector_push_lsp_list_ptr(&lst_stack, nlst);
                //fprintf(stderr, "AST: starting list!\n");
                break;
            }

            case T_LIST_END: {
                // Push either to the lst_stack OR directly to the AST,
                // see the PUSH_TOP_LIST_STACK_OR_AST marco.
                // This means that at the end the lst_stack will
                // empty as long as all lists are terminated.
                lsp_list *top = vector_pop_lsp_list_ptr(&lst_stack);
                PUSH_TOP_LIST_STACK_OR_AST(top);
                //fprintf(stderr, "AST: pushing list!\n");
                //lsp_obj_print_repr((lsp_obj *) top);
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
            case T_QUOTE:
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
                lsp_obj *evaluated = evaluate_list(lst);
                if (evaluated) {
                    vector_push_lsp_obj_ptr(&rlst->vec, evaluated);
                }
                break;
            }
        }
    }
    return rlst;
}

lsp_obj *evaluate_list(lsp_list *lst)
{
    if (lst->vec.len == 0) {
        // dont even bother
        return NULL;
    }

    lsp_obj *front = vector_get_lsp_obj_ptr(&lst->vec, 0);
    assert(front);

    //// evaluate all lists in the rest of the list
    //// before passing them to the function.
    //lsp_list evl_lst;
    //assert(!lsp_obj_init((lsp_obj *) &evl_lst, OBJ_LIST));

    //for (size_t i = 0; i < lst->vec.len; i++) {
    //    lsp_obj *o = vector_get_lsp_obj_ptr(&lst->vec, i);
    //    assert(o);

    //    if (o->type == OBJ_LIST) {
    //        //fprintf(stdout, "Evaluating: ");
    //        //lsp_obj_print_repr(o);
    //        lsp_obj *evaled_o = evaluate_list((lsp_list *) o);
    //        // might evaluate to NULL
    //        if (evaled_o) {
    //            //assert(evaled_o);
    //            vector_push_lsp_obj_ptr(&evl_lst.vec, evaled_o);
    //        }
    //    } else {
    //        lsp_obj *cloned = lsp_obj_clone((lsp_obj *) o);
    //        assert(cloned);
    //        vector_push_lsp_obj_ptr(&evl_lst.vec, cloned);
    //    }
    //}

    lsp_obj *ret = NULL;

    if (front->type == OBJ_SYMBOL) {
        // TODO actually to real symbol resolution
        // to make sure that it is a function and
        // not a variable/constant.
        lsp_symbol *symb = (lsp_symbol *) front;
        BUILTIN_FUNC_PTR(fptr) = builtin_get_func(symb->symb);
        if (fptr) {
            ret = fptr(lst);
            //ret = fptr(&evl_lst);
        } else {
            fprintf(stderr, "Runtime error: unable to resolve symbol: %s\n",
                    symb->symb);
            ret = NULL; //...
        }
    } else {
        // was not a function call, resolve to itself
        ret = lsp_obj_clone((lsp_obj *) lst);
    }

    // destroy the evaluated list
    //assert(!lsp_obj_destroy((lsp_obj *) &evl_lst));

    return ret;
}
