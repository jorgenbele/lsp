#define _XOPEN_SOURCE 700

#include <stdio.h>
#include <assert.h>
#include <time.h>
#include <stddef.h>
#include "builtins.h"
#include "interp.h"
#include "utils.h"

// static functions
static int set_symbol(lsp_symbol *symb, bool update, bool ignore_existing);

interp_ctx global_interp_ctx;

DEF_VECTOR_FUNCS(interp_state, interp_state, ((interp_state) {NORMAL, 0}))
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
DEF_VECTOR_FUNCS(lsp_list_ptr, lsp_list_ptr, NULL)


int interp_init()
{
    return interp_ctx_init(&global_interp_ctx);
}

int interp_destroy()
{
    struct timespec stop;
    assert(!clock_gettime(CLOCK_MONOTONIC, &stop));
    int64_t diff_nsec = stop.tv_nsec - global_interp_ctx.start.tv_nsec;
    int64_t diff_sec = stop.tv_sec - global_interp_ctx.start.tv_sec;
    diff_sec += (int64_t) (diff_nsec / 10e8);
    diff_nsec = diff_nsec % (int64_t) 10e8;

    //fprintf(stderr, "destroying interp context!\n");
    fprintf(stderr, "finished in %lu s %lu ns\n", diff_sec, diff_nsec);
    fprintf(stderr, "%lu obj heap allocs, %lu obj destroys, %lu evals\n",
            global_interp_ctx.n_obj_heap_new, global_interp_ctx.n_obj_destroy,
            global_interp_ctx.n_obj_eval);
    lsp_obj_pool_print_stats();
    return interp_ctx_destroy(&global_interp_ctx);
}

// Create the abstract syntax tree from the provided
// tokens. The AST is itself stored as a lsp_lst,
// which means that the input (print 32 32 "123") will
// itself print to (print 32 32 "123").

// does not modify the contents in tokens,
// except that tokens->error may change.
// {SYMBOL PRINT, INT 24, FLOAT 32, STRING TEST} --> (print 24 32 TEST)
// Creates the abstract syntax tree for a given object
lsp_list *ast_build(vector_token *tokens)
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
#define PUSH_TOP_LIST_STACK_OR_AST(ptr)                         \
    do {                                                        \
        lsp_list *below = vector_peek_lsp_list_ptr(&lst_stack); \
        if (!below) {                                           \
            below = ast;                                        \
        }                                                       \
        vector_push_lsp_obj_ptr(&below->vec, (lsp_obj *) ptr);  \
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
                // skip
                break;
        }
    }

    if (lst_stack.len > 0) {
        // Destroy the lst_stack and the ast
        while (lst_stack.len > 0 && !lst_stack.error) {
            lsp_list *l = vector_pop_lsp_list_ptr(&lst_stack);
            if (!l) {
                break;
            }
            assert(l);
            assert(!lsp_obj_destroy((lsp_obj *) l));
            assert(!lsp_obj_pool_release_obj((lsp_obj *) l));
            //free(l);
        }
        vector_destroy_lsp_list_ptr(&lst_stack);
        lsp_obj_destroy((lsp_obj *) ast);
        //free(ast);
        lsp_obj_pool_release_obj((lsp_obj *) ast);

        fprintf(stderr, "Runtime error: missing parenthesis?\n");
        return NULL;
    }

    // Destroy the lst_stack vector (not its contents)
    vector_destroy_lsp_list_ptr(&lst_stack);

    return ast;
}

int interp_ctx_init(interp_ctx *ctx)
{
    memset(ctx, 0, sizeof (*ctx));
    //assert(!lsp_obj_init((lsp_obj *) &ctx->symbols, OBJ_LIST));
    //ctx->symbols = (lsp_list *) lsp_obj_pool_take_obj();
    //assert(ctx->symbols);
    assert(!vector_init_lsp_list_ptr(&ctx->symbols_stack));

    lsp_list *symbols = (lsp_list *) lsp_obj_pool_take_obj();
    assert(symbols);
    lsp_obj_init((lsp_obj *) symbols, OBJ_LIST);
    assert(symbols->type == OBJ_LIST);
    assert(!vector_push_lsp_list_ptr(&ctx->symbols_stack, symbols));

    assert(!clock_gettime(CLOCK_MONOTONIC, &ctx->start));
    return 0;
}

int interp_ctx_destroy(interp_ctx *ctx)
{
    while (ctx->symbols_stack.len > 0) {
        lsp_list *symbols = vector_pop_lsp_list_ptr(&ctx->symbols_stack);
        if (ctx->symbols_stack.error) {
            break;
        }
        assert(symbols);
        assert(!lsp_obj_pool_release_obj((lsp_obj *) symbols));
    }

    //assert(!lsp_obj_destroy((lsp_obj *) &ctx->symbols));
    //assert(!lsp_obj_pool_release_obj((lsp_obj *) ctx->symbols));
    //free(ctx->symbols);
    assert(!vector_destroy_lsp_list_ptr(&ctx->symbols_stack));
    return 0;
}

lsp_list *ast_execute(lsp_list *ast)
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
            case OBJ_GENERIC: {
                // resolve to itself
                lsp_obj *clone = lsp_obj_clone(obj);
                assert(clone);
                vector_push_lsp_obj_ptr(&rlst->vec, clone);
                break;
            }

            case OBJ_SYMBOL: {
                lsp_symbol *symb = (lsp_symbol *) obj;
                lsp_obj *evaluated = lsp_symbol_eval(symb);
                if (evaluated) {
                    vector_push_lsp_obj_ptr(&rlst->vec, evaluated);
                }
                break;
            }

            case OBJ_LIST: {
                // if the first element of the list is
                // a symbol, then execute the function call
                // on the symbol and push the result
                lsp_list *lst = (lsp_list *) obj;
                lsp_obj *evaluated = list_evaluate(lst);
                if (evaluated) {
                    vector_push_lsp_obj_ptr(&rlst->vec, evaluated);
                }
                break;
            }
        }
    }
    return rlst;
}

static int set_symbol(lsp_symbol *symb, bool update, bool ignore_existing)
{
    // make sure it does not already exist
    lsp_list *symbols = vector_peek_lsp_list_ptr(&global_interp_ctx.symbols_stack);
    assert(symbols);
    assert(symbols->type == OBJ_LIST);
    //size_t len = lsp_list_len(global_interp_ctx.symbols);
    size_t len = lsp_list_len(symbols);
    for (size_t i = len; i > 0; i--) {
        //const lsp_obj *e_obj = lsp_list_get(&global_interp_ctx.symbols, i);
        // unsafe
        lsp_obj *e_obj = vector_get_lsp_obj_ptr(
            //&global_interp_ctx.symbols->vec, i
            &symbols->vec, i-1
        );
        assert(e_obj);
        assert(e_obj->type == OBJ_SYMBOL);
        lsp_symbol *e_symb = (lsp_symbol *) e_obj;
        bool exists = (e_symb->symb_len == symb->symb_len
                       && !strncmp(e_symb->symb, symb->symb, e_symb->symb_len));
        if (!ignore_existing) {
            if (exists && !update) {
                fprintf(stderr, "Runtime error: trying to redefine symbol `%s`.\n",
                        symb->symb);
                return 1;
            } else if (!exists && update) {
                fprintf(stderr, "Runtime error: trying to update non-existing symbol `%s`.\n",
                        symb->symb);
                return 1;
            }
        }

        if (exists) {
            if (update) {
                assert(!lsp_obj_destroy(e_obj));
                //free(e_obj);
                assert(!lsp_obj_pool_release_obj(e_obj));
                //return vector_set_lsp_obj_ptr(&global_interp_ctx.symbols->vec,
                //                              (lsp_obj *) symb, i);
                fprintf(stderr, "** updating existing **\n");
                return vector_set_lsp_obj_ptr(&symbols->vec, (lsp_obj *) symb, i-1);
            }
        }

        //existing = e_symb;
        //assert(e_obj->type == OBJ_SYMBOL);
        //lsp_symbol *e_symb = (lsp_symbol *) e_obj; // unsafe
        //bool exists = (e_symb->symb_len == symb->symb_len
        //               && !strncmp(e_symb->symb, symb->symb, e_symb->symb_len));
        //if (!ignore_existing) {
        //    if (exists && !update) {
        //        fprintf(stderr, "Runtime error: trying to redefine symbol `%s`.\n",
        //                symb->symb);
        //        return 1;
        //    } else if (!exists && update) {
        //        fprintf(stderr, "Runtime error: trying to update non-existing symbol `%s`.\n",
        //                symb->symb);
        //        return 1;
        //    }
        //}
        //existing = e_symb;
    }

    //if (update && existing) {
    //    assert(!lsp_obj_destroy((lsp_obj *) existing));
    //    // modify inplace
    //    memcpy(existing, symb, sizeof (*symb));
    //}

    // does not already exist, push to list at the top of the stack
    //return lsp_list_push(global_interp_ctx.symbols, (lsp_obj *) symb);
    //return lsp_list_push(symbols, (lsp_obj *) symb);
    fprintf(stderr, "** pushing symbol ");
    lsp_obj_print_repr((lsp_obj *) symb);
    lsp_list *top_symbols = vector_peek_lsp_list_ptr(&global_interp_ctx.symbols_stack);
    assert(top_symbols);
    int ret = lsp_list_push(top_symbols, (lsp_obj *) symb);
    fprintf(stderr, "** top of symbol stack: ");
    lsp_obj_print_repr((lsp_obj *) top_symbols);
    return ret;
}

/*
  (defun <funcname> <arglist> <atom>...)
 * Example
 * (defun test (arg1 arg2)
 *      (print arg1)
 *      (print arg2))
 *
 * (test "Hello" ", World") => prints "Hello, World"
 */
lsp_obj *evaluate_defun(lsp_list *argl)
{
    //assert(!"defun is not implemented yet");
    //assert(lsp_list_len(lst) >= 2);
    //lsp_obj *funcname = vector_get_lsp_obj_ptr(&lst->vec, 1);
    //assert(funcname && funcname->type == OBJ_SYMBOL);
    //lsp_symbol *symb = (lsp_symbol *) funcname;

    REQUIRES_ATLEAST_N_ARGS("defun", argl, 3);

    const lsp_obj *var = lsp_list_get(argl, 1);
    assert(var);
    assert(var->type == OBJ_SYMBOL);

    lsp_symbol *symb = (lsp_symbol *) lsp_obj_clone(var);
    assert(symb);
    symb->func = true;

    lsp_list *rest = lsp_list_after(argl, 1);
    assert(rest);
    fprintf(stderr, "rest:");
    lsp_obj_print_repr(rest);
    symb->val = (lsp_obj *) rest;
    assert(!set_symbol(symb, true, true));

    fprintf(stderr, "symb:");
    lsp_obj_print_repr(symb);

    return lsp_obj_new(OBJ_GENERIC);
}

// (setq <name> <value>)
// sets a previously defined symbol with the
// name <name> to have the  value <value>
lsp_obj *evaluate_setq(lsp_list *argl)
{
    REQUIRES_N_ARGS("setq", argl, 2);

    const lsp_obj *var = lsp_list_get(argl, 1);
    assert(var);
    assert(var->type == OBJ_SYMBOL);

    lsp_symbol *symb = (lsp_symbol *) lsp_obj_clone(var);
    assert(symb);

    lsp_obj *value = lsp_list_get_eval(argl, 2); // NOTE: uses eval
    assert(value);
    symb->val = value;
    assert(!set_symbol(symb, true, true));
    return lsp_obj_new(OBJ_GENERIC);
}

static lsp_obj *execute_builtin(lsp_symbol *symb, lsp_list *lst)
{
//fprintf(stderr, "**execute builtin**\n");
    BUILTIN_FUNC_PTR(fptr) = builtin_get_func(symb->symb);
    if (fptr) {
        return fptr(lst);
    }

    if (!strcmp(symb->symb, "defun")) {
        fprintf(stderr, "**evaluating defun**\n");
        return evaluate_defun(lst);
    } else if (!strcmp(symb->symb, "setq")) {
        fprintf(stderr, "**evaluating setq**\n");
        return evaluate_setq(lst);
    }
    return NULL;
}

lsp_obj *execute_defun_func(lsp_symbol *symb, lsp_list *argl)
{
    assert(symb->func); // cannot execute symbol that is

    fprintf(stderr, "** exec argl:");
    lsp_obj_print_repr(argl);

    fprintf(stderr, "** exec symb:");
    lsp_obj_print_repr(symb);

    // get the function
    lsp_obj *func = lsp_symbol_eval(symb);
    assert(func);
    fprintf(stderr, "** exec func:");
    lsp_obj_print_repr(func);

    // create and use a new "block" scope
    lsp_list *symbols = (lsp_list *) lsp_obj_new(OBJ_LIST);
    assert(symbols);
    fprintf(stderr, "** symbols:");
    lsp_obj_print_repr(symbols);

    // set the symbols listed in the funcs args (index 1 in the symb->vals list)
    // to the corresponding value in argl in the new "block" scope
    lsp_list *defl = (lsp_list *) symb->val;
    assert(defl);
    assert(defl->type == OBJ_LIST);
    fprintf(stderr, "** defl:");
    lsp_obj_print_repr(defl);

    lsp_list *argdefl = (lsp_list *) lsp_list_get(defl, 1);
    assert(argdefl);
    assert(argdefl->type == OBJ_LIST);
    fprintf(stderr, "** argdefl:");
    lsp_obj_print_repr(argdefl);

    size_t argdefl_len = lsp_list_len(argdefl);
    size_t argl_len = lsp_list_len(argl);
    fprintf(stderr, "** argdefl: %lu, argl: %lu\n", argdefl_len, argl_len);

    assert(argdefl_len == argl_len - 1); // make sure all arguments are passed
    for (size_t i = 0; i < argdefl_len; i++) {
        lsp_symbol *symb_def = (lsp_symbol *) lsp_list_get(argdefl, i);
        assert(symb_def && symb_def->type == OBJ_SYMBOL);
        fprintf(stderr, "** symb_def:");
        lsp_obj_print_repr(symb_def);

        lsp_obj *arg = lsp_list_get(argl, i+1);
        assert(arg);
        fprintf(stderr, "** arg:");
        lsp_obj_print_repr(arg);

        lsp_symbol *symb_clone = (lsp_symbol *) lsp_obj_clone((lsp_obj *) symb_def);
        assert(symb_clone && symb_clone->type == OBJ_SYMBOL);

        fprintf(stderr, "** symb_clone:");
        lsp_obj_print_repr(symb_clone);

        lsp_obj *arg_clone = (lsp_obj *) lsp_obj_clone((lsp_obj *) arg);
        assert(arg_clone);

        fprintf(stderr, "** arg_clone:");
        lsp_obj_print_repr(arg_clone);

        symb_clone->val = arg_clone;

        // finally push the new symbol with the value provided in the argument list
        // to the symbols list
        assert(!lsp_list_push(symbols, (lsp_obj *) symb_clone));
    }

    fprintf(stderr, "** SYMBOLS STACK: ");
    lsp_obj_print_repr(symbols);

    // push the new symbols to the top of the symbols_stack
    assert(!vector_push_lsp_list_ptr(&global_interp_ctx.symbols_stack, symbols));

    // evaluate the function (actually only the symb->vals list) with
    // the new symbols at the top of the symbols stack.
    fprintf(stderr, "** SYMB->val: ");
    lsp_obj_print_repr(symb->val);
    assert(symb->val->type == OBJ_LIST);
    lsp_obj *func_s_expr = lsp_list_get((lsp_list *) symb->val, 2);
    assert(func_s_expr);
    //lsp_obj *res = lsp_obj_eval(symb->val);
    lsp_obj *res = lsp_obj_eval(func_s_expr);
    assert(res);

    // now pop and destroy the top of the symbols_stack.
    lsp_list *symbols_fs = vector_pop_lsp_list_ptr(&global_interp_ctx.symbols_stack);
    assert(symbols_fs);
    lsp_obj_destroy((lsp_obj *) symbols_fs);
    lsp_obj_pool_release_obj((lsp_obj *) symbols_fs);
    
    // return the result
    return res;
}

lsp_obj *list_evaluate(lsp_list *lst)
{
    if (lst->vec.len == 0) {
        // dont even bother, return empty list
        return lsp_obj_new(OBJ_LIST);
    }

    const lsp_obj *front = vector_get_lsp_obj_ptr(&lst->vec, 0);
    assert(front);

    lsp_obj *ret = NULL;

    if (front->type == OBJ_SYMBOL) {
        // evaluate the symbol
        lsp_obj *eval = lsp_symbol_eval((lsp_symbol *) front);
        assert(eval);

        if (eval->type == OBJ_SYMBOL) {
            lsp_symbol *e_symb = (lsp_symbol *) eval;
            // check if function (defined using defun)
            if (e_symb->func) {
                fprintf(stderr, "*** list_evaluate executing defun-function: ");
                lsp_obj_print_repr(e_symb);
                fprintf(stderr, ", lst, ");
                lsp_obj_print_repr(lst);
                lsp_obj *rlst = execute_defun_func(e_symb, lst);
                if (rlst) {
                    ret = rlst;
                }
            } else {
                // check if builtin
                fprintf(stderr, "*** list_evaluate executing builtin-function! ***\n");
                lsp_obj *rlst = execute_builtin(e_symb, lst);
                if (rlst) {
                    ret = rlst;
                }
            }

            lsp_obj_destroy(eval);
            lsp_obj_pool_release_obj(eval);
            //free(eval);
        } else {
            ret = eval;
        }

    } else {
        // was not a function call, resolve to itself
        ret = lsp_obj_clone((lsp_obj *) lst);
    }

ret_:
    return ret;
}
