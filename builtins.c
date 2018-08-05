#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include <stdbool.h>

#include "builtins.h"
#include "utils.h"
#include "types.h"

const builtin builtins[] = {
    {"print", builtin_print},
    {"sum", builtin_number_sum},
    {"+", builtin_number_sum},
    {"-", builtin_number_minus},
    {"repr", builtin_repr},
    {NULL, NULL},
};

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
