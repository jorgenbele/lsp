#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include <stdbool.h>

#include "builtins.h"
#include "utils.h"
#include "types.h"
#include "interp.h"

const builtin builtins[] = {
    {"print", builtin_print},
    {"sum", builtin_number_sum},
    {"+", builtin_number_sum},
    {"-", builtin_number_minus},
    {"*", builtin_number_mult},
    {"if", builtin_if},
    {"repr", builtin_repr},
    {"list", builtin_list},
    {"eval", builtin_eval},
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

// evalates the arguments
// (eval <list>) -> <result>
// example: (eval (list + 1 2 3)) = 6
lsp_obj *builtin_eval(vector_lsp_obj_ptr *argv)
{
    if (!argv) {
        fprintf(stderr, "Runtime error: failed to run `eval`, missing arguments!\n");
        exit(1);
    } else if (argv->len > 2) {
        fprintf(stderr, "Runtime error: failed to run `eval`, takes maximum 1 arguments!\n");
        exit(1);
    }
    lsp_obj *obj = vector_get_lsp_obj_ptr(argv, 1);
    assert(obj);
    assert(obj->type == OBJ_LIST);
    lsp_list *lst = (lsp_list *) obj;
    return evaluate_list(lst);
}

// returns a list of its arguments
lsp_obj *builtin_list(vector_lsp_obj_ptr *argv)
{
    if (!argv || argv->len < 2) {
        // Empty list
        return lsp_obj_new(OBJ_LIST);
    }

    lsp_list *lst =  (lsp_list *) lsp_obj_new(OBJ_LIST);
    assert(lst);
    for (size_t i = 1; i < argv->len; i++) {
        lsp_obj *obj = vector_get_lsp_obj_ptr(argv, i);
        assert(obj);
        lsp_obj *clone = lsp_obj_clone(obj);
        assert(clone);
        assert(!vector_push_lsp_obj_ptr(&lst->vec, clone));
    }
    return (lsp_obj *) lst;
}


// (if <condition> <on-condition-true> <on-condition-false>)
lsp_obj *builtin_if(vector_lsp_obj_ptr *argv)
{
    if (!argv) {
        fprintf(stderr, "Runtime error: failed to run `if`, missing arguments!\n");
        exit(1);
    } else if (argv->len < 3) {
        fprintf(stderr, "Runtime error: failed to run `if`, requires at least 2 arguments!\n");
        exit(1);
    } else if (argv->len > 4) {
        fprintf(stderr, "Runtime error: failed to run `if`, takes maximum 4 arguments!\n");
        exit(1);
    }

    lsp_obj *condition = vector_get_lsp_obj_ptr(argv, 1);
    lsp_obj *to_evaluate = NULL;

    if (lsp_obj_is_true(condition)) {
        to_evaluate = lsp_obj_clone(vector_get_lsp_obj_ptr(argv, 2));
    } else if (argv->len == 4) {
        to_evaluate = lsp_obj_clone(vector_get_lsp_obj_ptr(argv, 3));
    }
    if (!to_evaluate) {
        return NULL;
    }

    // evaluate only if list
    if (to_evaluate->type == OBJ_LIST) {
        lsp_obj *ret = evaluate_list((lsp_list *) to_evaluate);
        lsp_obj_destroy(to_evaluate);
        free(to_evaluate);
        return ret;
    }
    return to_evaluate;
}



lsp_obj *builtin_number_minus(vector_lsp_obj_ptr *argv)
{
    if (!argv) {
        fprintf(stderr, "Runtime error: failed to run `-`, missing arguments!\n");
        exit(1);
    } else if (argv->len != 3) {
        fprintf(stderr, "Runtime error: failed to run `-`, requires 2 arguments!\n");
        exit(1);
    }

    // x - y
    lsp_obj *x = vector_get_lsp_obj_ptr(argv, 1);
    lsp_obj *y = vector_get_lsp_obj_ptr(argv, 2);

    bool using_float = false;
    int64_t int_result = 0;
    double flt_result = 0;

    if (x->type == OBJ_INT) {
        int_result = x->integer;
    } else if (x->type == OBJ_FLOAT) {
        using_float = true;
        flt_result = x->flt;
    } else {
        fprintf(stderr, "Runtime error: failed to run `-`, requres numeric args!\n");
        exit(1);
    }

    if (y->type == OBJ_INT) {
        int_result -= y->integer;
    } else if (y->type == OBJ_FLOAT) {
        using_float = true;
        flt_result -= y->flt;
    } else {
        fprintf(stderr, "Runtime error: failed to run `-`, requres numeric args!\n");
        exit(1);
    }

    lsp_obj *res = xmalloc(sizeof(*res));
    if (using_float) {
        lsp_obj_init(res, OBJ_FLOAT);
        res->flt = flt_result + int_result;
    } else {
        lsp_obj_init(res, OBJ_INT);
        res->integer = int_result;
    }
    return res;
}

lsp_obj *builtin_repr(vector_lsp_obj_ptr *argv)
{
    if (!argv) {
        return NULL;
    }

    lsp_str *lstr = xcalloc(1, sizeof (*lstr));
    lsp_str_init(lstr);

    char *buf = NULL;
    size_t buf_s = 0;

    for (size_t i = 1; i < argv->len; i++) {
        lsp_obj *obj = vector_get_lsp_obj_ptr(argv, i);
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

        if (i + 1 < argv->len) {
            lsp_str_cat_n(lstr, " ", 1);
        }
    }

    free(buf);
    return (lsp_obj *) lstr;
}

lsp_obj *builtin_print(vector_lsp_obj_ptr *argv)
{
    if (!argv) {
        return NULL;
    }

    // The first ptr in argv is the symbol which was used to call this
    // function. Skip it.
    for (size_t i = 1; i < argv->len; i++) {
        lsp_obj *obj = vector_get_lsp_obj_ptr(argv, i);
        assert(obj);
        lsp_obj_print(obj);
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
                fprintf(stderr, "Runtime error: `sum` expected number, got %s!\n", obj_type_str[ptr->type]);
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

lsp_obj *builtin_number_mult(vector_lsp_obj_ptr *argv)
{
    if (!argv || argv->len < 2) {
        // 0
        lsp_obj *res = xmalloc(sizeof(*res));
        lsp_obj_init(res, OBJ_INT);
        res->integer = 0;
        return res;
    }

    bool using_float = false;
    int64_t int_sum = 1;
    double flt_sum = 1;

    for (size_t i = 1; i < argv->len; i++) {
        const lsp_obj *ptr = vector_get_lsp_obj_ptr(argv, i);
        assert(!argv->error);
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
