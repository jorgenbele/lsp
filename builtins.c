#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include <stdbool.h>

#include "builtins.h"
#include "utils.h"
#include "types.h"
#include "interp.h"

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

const builtin builtins[] = {
    {"print", builtin_print},
    {"println", builtin_println},
    {"+", builtin_number_sum},
    {"-", builtin_number_minus},
    {"*", builtin_number_mult},
    {"if", builtin_if},
    {"quote", builtin_quote},
    {"repr", builtin_repr},
    {"eval", builtin_eval},
    {"repeat", builtin_repeat},
    {NULL, NULL},
};

void *builtin_get_func(const char *name)
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

lsp_obj *builtin_print(lsp_list *argl)
{
    // The first ptr in argv is the symbol which was used to call this
    // function. Skip it.
    size_t argl_len = lsp_list_len(argl);
    for (size_t i = 1; i < argl_len; i++) {
        lsp_obj *obj = lsp_list_get_eval(argl, i);
        assert(obj);
        lsp_obj_print(obj);
        if (i + 1 < argl_len) {
            putchar(' ');
        }
        lsp_obj_destroy(obj);
        free(obj);
    }
    return NULL;
}

lsp_obj *builtin_println(lsp_list *argl)
{
    lsp_obj *obj = builtin_print(argl);
    putchar('\n');
    return obj;
}

//lsp_obj *builtin_number_sum(vector_lsp_obj_ptr *argv)
lsp_obj *builtin_number_sum(lsp_list *argl)
{
    bool using_float = false;
    int64_t int_sum = 0;
    double flt_sum = 0;

    size_t argl_len = lsp_list_len(argl);
    for (size_t i = 1; i < argl_len; i++) {
        lsp_obj *ptr = lsp_list_get_eval(argl, i);
        assert(!lsp_list_error(argl));
        switch (ptr->type) {
            case OBJ_INT:
                int_sum += ptr->integer;
                break;

            case OBJ_FLOAT:
                using_float = true;
                flt_sum += ptr->flt;
                break;

            default:
                fprintf(stderr, "Runtime error: `+` expected number, got %s!\n", obj_type_str[ptr->type]);
                exit(1);
                break;
        }
        lsp_obj_destroy(ptr);
        free(ptr);
    }

    lsp_obj *res = lsp_obj_new(using_float ? OBJ_FLOAT : OBJ_INT);
    assert(res);
    if (using_float) {
        res->flt = flt_sum + int_sum;
    } else {
        res->integer = int_sum;
    }
    return res;
}

// (- <y> <x1> <x2> ... <xn>) equals to y - x1 - x2 - x3 ... - xn
lsp_obj *builtin_number_minus(lsp_list *argl)
{
    bool using_float = false;

    int64_t int_sum = 0;
    double flt_sum = 0;

    size_t argl_len = lsp_list_len(argl);
    int sign = 1;

    for (size_t i = 1; i < argl_len; i++) {
        lsp_obj *ptr = lsp_list_get_eval(argl, i);
        assert(!lsp_list_error(argl));
        switch (ptr->type) {
            case OBJ_INT:
                int_sum += sign*ptr->integer;
                break;

            case OBJ_FLOAT:
                using_float = true;
                flt_sum += sign*ptr->flt;
                break;

            default:
                fprintf(stderr, "Runtime error: `-` expected number, got %s!\n", obj_type_str[ptr->type]);
                exit(1);
                break;
        }
        lsp_obj_destroy(ptr);
        free(ptr);

        sign = -1;
    }

    lsp_obj *res = lsp_obj_new(using_float ? OBJ_FLOAT : OBJ_INT);
    assert(res);
    if (using_float) {
        res->flt = flt_sum + int_sum;
    } else {
        res->integer = int_sum;
    }
    return res;
}

lsp_obj *builtin_number_mult(lsp_list *argl)
{
    REQUIRES_ATLEAST_N_ARGS("*", argl, 1);

    bool using_float = false;
    int64_t int_sum = 1;
    double flt_sum = 1;

    size_t len = lsp_list_len(argl);
    for (size_t i = 1; i < len; i++) {
        lsp_obj *ptr = lsp_list_get_eval(argl, i);
        //const lsp_obj *ptr = vector_get_lsp_obj_ptr(argv, i);
        assert(!lsp_list_error(argl));
        switch (ptr->type) {
            case OBJ_INT:
                int_sum *= ptr->integer;
                break;

            case OBJ_FLOAT:
                using_float = true;
                flt_sum *= ptr->flt;
                break;

            default:
                fprintf(stderr, "Runtime error: `*` expected number, got %s!\n", obj_type_str[ptr->type]);
                exit(1);
                break;
        }
        lsp_obj_destroy(ptr);
        free(ptr);
    }

    lsp_obj *res = xmalloc(sizeof(*res));
    if (using_float) {
        lsp_obj_init(res, OBJ_FLOAT);
        res->flt = flt_sum * int_sum;
    } else {
        lsp_obj_init(res, OBJ_INT);
        res->integer = int_sum;
    }
    return res;
}

// (if <condition> <on-condition-true> <on-condition-false>)
lsp_obj *builtin_if(lsp_list *argl)
{
    REQUIRES_ATLEAST_N_ARGS("if", argl, 2);
    REQUIRES_MAXIMUM_N_ARGS("if", argl, 4);

    lsp_obj *condition = lsp_list_get_eval(argl, 1); 
    lsp_obj *to_evaluate = NULL;

    if (lsp_obj_is_true(condition)) {
        to_evaluate = lsp_list_get(argl, 2);
    } else if (lsp_list_len(argl) == 4) {
        to_evaluate = lsp_list_get(argl, 3);
    }
    if (!to_evaluate) {
        // empty
        return lsp_obj_new(OBJ_LIST);
    }

    lsp_obj *evaluated = lsp_obj_eval(to_evaluate);
    return evaluated;
}

// (quote <obj>) => <obj>
lsp_obj *builtin_quote(lsp_list *argl)
{
    REQUIRES_MAXIMUM_N_ARGS("quote", argl, 1);
    lsp_obj *obj = lsp_list_get(argl, 1);
    assert(obj);
    lsp_obj *clone = lsp_obj_clone(obj);
    assert(clone);
    return clone;
}

lsp_obj *builtin_repr(lsp_list *argl)
{
    lsp_str *lstr = xcalloc(1, sizeof (*lstr));
    lsp_str_init(lstr);

    char *buf = NULL;
    size_t buf_s = 0;

    size_t argl_len = lsp_list_len(argl);
    for (size_t i = 1; i < argl_len; i++) {
        lsp_obj *obj = lsp_list_get_eval(argl, i);
        assert(obj);

        if (lsp_obj_repr_str(obj, &buf, &buf_s)) {
            free(buf);
            lsp_str_destroy(lstr);
            fprintf(stderr, "Runtime error: failed to run `repr`!\n");
            exit(1);
            //return NULL;
        }

        assert(!lsp_str_cat_n(lstr, buf, strlen(buf)));
        memset(buf, 0, buf_s);

        if (i + 1 < argl_len) {
            lsp_str_cat_n(lstr, " ", 1);
        }

        lsp_obj_destroy(obj);
        free(obj);
    }

    free(buf);
    return (lsp_obj *) lstr;
}


// evalates the arguments
// (eval <list>) -> <result>
// example: (eval (list + 1 2 3)) = 6
lsp_obj *builtin_eval(lsp_list *argl)
//lsp_obj *builtin_eval(vector_lsp_obj_ptr *argv)
{
    REQUIRES_MAXIMUM_N_ARGS("eval", argl, 1);
    REQUIRES_ATLEAST_N_ARGS("eval", argl, 1);
    // eval 2 times.
    lsp_obj *obj = lsp_list_get_eval(argl, 1);
    assert(obj);
    lsp_obj *eval_obj = lsp_obj_eval(obj);
    assert(eval_obj);
    lsp_obj_destroy(obj);
    free(obj);
    return eval_obj;
}

// repeats the argument n times
// (repeat n <list|item>) -> (<list|item> <list|item> ... n times)
// example: (repeat 3 (1)) = ((1) (1) (1))
// example: (repeat 1 1) = (1)
lsp_obj *builtin_repeat(lsp_list *argl)
//lsp_obj *builtin_repeat(vector_lsp_obj_ptr *argv)
{
    REQUIRES_ATLEAST_N_ARGS("repeat", argl, 2);
    REQUIRES_MAXIMUM_N_ARGS("repeat", argl, 2);

    lsp_obj *obj_n = lsp_list_get_eval(argl, 1);
    assert(obj_n);
    assert(!lsp_list_error(argl));
    lsp_obj *obj = lsp_list_get_eval(argl, 2);
    assert(obj);
    assert(!lsp_list_error(argl));

    lsp_list *lst = (lsp_list *) lsp_obj_new(OBJ_LIST);
    assert(lst);

    if (obj_n->integer > 0) {
        lsp_list_push(lst, obj); // DO NOT DESTROY/free obj
    }

    for (int64_t i = 1; i < obj_n->integer; i++) {
        lsp_obj *clone = lsp_obj_clone(obj);
        assert(clone);
        assert(!vector_push_lsp_obj_ptr(&lst->vec, clone));
    }

    lsp_obj_destroy(obj_n);
    free(obj_n);

    if (obj_n->integer == 0) {
        lsp_obj_destroy(obj);
        free(obj);
    }

    //fprintf(stderr, "list:\n");
    //lsp_obj_print_repr((lsp_obj *) lst);
    return (lsp_obj *) lst;
}
