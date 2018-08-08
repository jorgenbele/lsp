#include <stdio.h>
#include <assert.h>

#include "builtins.h"
#include "interp.h"
#include "utils.h"

// static functions
static int set_symbol(lsp_symbol *symb);
interp_ctx global_interp_ctx;

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


int interp_init()
{
    return interp_ctx_init(&global_interp_ctx);
}

int interp_destroy()
{
    fprintf(stderr, "destroying interp context!\n");
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

int interp_ctx_init(interp_ctx *ctx)
{
    memset(ctx, 0, sizeof (*ctx));
    assert(!lsp_obj_init((lsp_obj *) &ctx->symbols, OBJ_LIST));


    return 0;
}

int interp_ctx_destroy(interp_ctx *ctx)
{
    assert(!lsp_obj_destroy((lsp_obj *) &ctx->symbols));
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

static int set_symbol(lsp_symbol *symb)
{
    // make sure it does not already exist
    size_t len = lsp_list_len(&global_interp_ctx.symbols);
    for (size_t i = 0; i < len; i++) {
        const lsp_obj *e_obj = lsp_list_get(&global_interp_ctx.symbols, i);
        assert(e_obj->type == OBJ_SYMBOL);
        const lsp_symbol *e_symb = (lsp_symbol *) e_obj;
        if (e_symb->symb_len == symb->symb_len
            && !strncmp(e_symb->symb, symb->symb, e_symb->symb_len)) {
            fprintf(stderr, "Runtime error: trying to redefine symbol `%s`.\n",
                    symb->symb);
            return 1;
        }
    }
    return lsp_list_push(&global_interp_ctx.symbols, (lsp_obj *) symb);
}

/*
 * Example
 * (defun test (arg1 arg2)
 *      (print arg1)
 *      (print arg2))
 *
 * (test "Hello" ", World") => prints "Hello, World"
 * 
 */
lsp_obj *evaluate_defun(lsp_list *argl)
{
    assert(!"defun is not implemented yet");
    //assert(lsp_list_len(lst) >= 2);
    //lsp_obj *funcname = vector_get_lsp_obj_ptr(&lst->vec, 1);
    //assert(funcname && funcname->type == OBJ_SYMBOL);
    //lsp_symbol *symb = (lsp_symbol *) funcname;
    return NULL;
}

// (defvar <name> <value>)
// creates a new symbol with the name <name> and initial value <value>
lsp_obj *evaluate_defvar(lsp_list *argl)
{
    REQUIRES_N_ARGS("defvar", argl, 2);

    // NOTE: both var_eval and value are pushed to the symbol list
    lsp_obj *var_eval = lsp_list_get_eval(argl, 1); // NOTE: uses eval
    assert(var_eval);
    assert(var_eval->type == OBJ_SYMBOL);

    lsp_symbol *symb = (lsp_symbol *) var_eval;

    lsp_obj *value = lsp_list_get_eval(argl, 2); // NOTE: uses eval
    assert(value);
    symb->val = value;
    assert(!set_symbol(symb));
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
    } else if (!strcmp(symb->symb, "defvar")) {
        fprintf(stderr, "**evaluating defvar**\n");
        return evaluate_defvar(lst);
    }
    return NULL;
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
            // check if builtin
            lsp_obj *rlst = execute_builtin((lsp_symbol *) eval, lst);
            lsp_obj_destroy(eval);
            free(eval);
            if (rlst) {
                ret = rlst;
            }
        } else {
            ret = eval;
        }

    } else {
        // was not a function call, resolve to itself
        ret = lsp_obj_clone((lsp_obj *) lst);
    }

    return ret;
}
