#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include <stdbool.h>

#include "builtins.h"
#include "utils.h"
#include "types.h"
#include "interp.h"
#include "repl.h"

const builtin builtins[] = {
    {"progn", builtin_progn},
    {"print", builtin_print},
    {"println", builtin_println},

    // comparison
    {"eql", builtin_obj_eql},
    {"=", builtin_obj_eql},
    {"cmp", builtin_obj_cmp},
    {"lt", builtin_obj_lt},
    {"<", builtin_obj_lt},
    {"le", builtin_obj_le},
    {"<=", builtin_obj_le},
    {"gt", builtin_obj_gt},
    {">", builtin_obj_gt},
    {"ge", builtin_obj_ge},
    {">=", builtin_obj_ge},
    {"not", builtin_int_not},
    {"!", builtin_int_not},

    // arithmetic
    {"+", builtin_number_sum},
    {"-", builtin_number_minus},
    {"*", builtin_number_mult},

    // misc
    {"if", builtin_if},
    {"quote", builtin_quote},
    {"repr", builtin_repr},
    {"eval", builtin_eval},
    {"repeat", builtin_repeat},
    {"load-file", builtin_loadfile},

    // lists
    {"len", builtin_len},
    {"cons", builtin_cons},
    {"car", builtin_car},
    {"cdr", builtin_cdr},
    {"append", builtin_append},
    {"reverse", builtin_reverse},
    {"list", builtin_list},
    {NULL, NULL},
};

//void *builtin_get_func(const char *name)
builtin_func_ptr builtin_get_func(const char *name)
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

lsp_obj *builtin_progn(lsp_list *argl)
{
    size_t argl_len = lsp_list_len(argl);
    lsp_obj *prev = NULL;
    for (size_t i = 1; i < argl_len; i++) {
        lsp_obj *obj = lsp_list_get_eval(argl, i);
        if (prev) {
            assert(!lsp_obj_destroy(prev));
            assert(!lsp_obj_pool_release_obj(prev));
        }
        prev = obj;
    }
    return prev;
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
        //free(obj);
        lsp_obj_pool_release_obj(obj);
    }
    return lsp_obj_new(OBJ_GENERIC);
}

lsp_obj *builtin_println(lsp_list *argl)
{
    lsp_obj *obj = builtin_print(argl);
    putchar('\n');
    return obj;
}

lsp_obj *builtin_int_not(lsp_list *argl)
{
    REQUIRES_N_ARGS("not", argl, 1);

    lsp_obj *obj_eval = lsp_list_get_eval(argl, 1);
    assert(obj_eval);
    bool not = !lsp_obj_is_true(obj_eval);
    lsp_obj_pool_release_obj(obj_eval);

    lsp_obj *obj_not = lsp_obj_new(OBJ_INT);
    assert(obj_not);
    obj_not->integer = not;
    return obj_not;
}

#define CMP_BUILTIN(name, res, condition, expected_type)    \
    lsp_obj *builtin_obj_##name(lsp_list *argl)             \
    {                                                       \
        REQUIRES_ATLEAST_N_ARGS(#name, argl, 2);            \
        lsp_obj *res = builtin_obj_cmp(argl);               \
        assert(res);                                        \
        assert(res->type == OBJ_INT);                       \
        res->integer = condition;                           \
        return res;                                         \
    }

CMP_BUILTIN(gt, res, res->integer > 0, OBJ_INT)
CMP_BUILTIN(ge, res, res->integer >= 0, OBJ_INT)
CMP_BUILTIN(lt, res, res->integer < 0, OBJ_INT)
CMP_BUILTIN(le, res, res->integer <= 0, OBJ_INT)
CMP_BUILTIN(eql, res, res->integer == 0, OBJ_INT)

#undef CMP_BUILTIN

lsp_obj *builtin_obj_cmp(lsp_list *argl)
{
    REQUIRES_ATLEAST_N_ARGS("cmp", argl, 2);

    int cmp = 0;

    lsp_obj *first = lsp_list_get_eval(argl, 1);
    assert(first);

    size_t argl_len = lsp_list_len(argl);
    for (size_t i = 2; !cmp && i < argl_len; i++) {
        lsp_obj *o = lsp_list_get_eval(argl, i);
        cmp = lsp_obj_cmp(first, o);
        assert(!lsp_obj_pool_release_obj(o));
    }
    assert(!lsp_obj_pool_release_obj(first));

    lsp_obj *res = lsp_obj_new(OBJ_INT);
    assert(res);
    res->integer = cmp;
    return res;
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
        lsp_obj_pool_release_obj(ptr);
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
        lsp_obj_pool_release_obj(ptr);

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
        lsp_obj_pool_release_obj(ptr);
    }

    lsp_obj *res = lsp_obj_pool_take_obj();
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

    lsp_obj *ret = NULL;
    if (!to_evaluate) {
        // empty
         ret = lsp_obj_new(OBJ_GENERIC);
    } else {
        ret = lsp_obj_eval(to_evaluate);
    }

    lsp_obj_destroy(condition);
    lsp_obj_pool_release_obj(condition);

    return ret;
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
    lsp_str *lstr = (lsp_str *) lsp_obj_pool_take_obj();
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
        }

        assert(!lsp_str_cat_n(lstr, buf, strlen(buf)));
        memset(buf, 0, buf_s);

        if (i + 1 < argl_len) {
            lsp_str_cat_n(lstr, " ", 1);
        }

        lsp_obj_destroy(obj);
        lsp_obj_pool_release_obj(obj);
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
    lsp_obj_pool_release_obj(obj);
    return eval_obj;
}

// repeats the argument n times
// (repeat n <list|item>) -> (<list|item> <list|item> ... n times)
// example: (repeat 3 (1)) = ((1) (1) (1))
// example: (repeat 1 1) = (1)
lsp_obj *builtin_repeat(lsp_list *argl)
{
    REQUIRES_ATLEAST_N_ARGS("repeat", argl, 2);
    REQUIRES_MAXIMUM_N_ARGS("repeat", argl, 2);

    lsp_obj *obj_n = lsp_list_get_eval(argl, 1);
    assert(obj_n);
    assert(obj_n->type == OBJ_INT);
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

    if (obj_n->integer == 0) {
        lsp_obj_destroy(obj);
        lsp_obj_pool_release_obj(obj);
    }

    lsp_obj_destroy(obj_n);
    lsp_obj_pool_release_obj(obj_n);
    return (lsp_obj *) lst;
}

lsp_obj *builtin_len(lsp_list *argl)
{
    REQUIRES_N_ARGS("len", argl, 1);
    lsp_obj *arg = lsp_list_get_eval(argl, 1);
    assert(arg && arg->type == OBJ_LIST);
    int64_t len = lsp_list_len((lsp_list *) arg);
    lsp_obj *int_obj = lsp_obj_new(OBJ_INT);
    int_obj->integer = len;
    lsp_obj_destroy(arg);
    lsp_obj_pool_release_obj(arg);
    return int_obj;
}

// NOTE that this is only for adding to the front
// of a list, nothing else
lsp_obj *builtin_cons(lsp_list *argl)
{
    REQUIRES_N_ARGS("cons", argl, 2);
    lsp_obj *obj = lsp_list_get_eval(argl, 1);
    assert(obj);
    lsp_list *lst = (lsp_list *) lsp_list_get_eval(argl, 2);
    assert(lst && lst->type == OBJ_LIST);

    lsp_list *new_lst = (lsp_list *) lsp_obj_new(OBJ_LIST);
    assert(new_lst);

    lsp_list_push(new_lst, obj);

    size_t lst_len = lsp_list_len(lst);
    for (size_t i = 0; i < lst_len; i++) {
        lsp_obj *o = lsp_list_get(lst, i);
        assert(o && !lsp_list_error(lst));
        lsp_list_push(new_lst, o);
    }

    lsp_obj_destroy((lsp_obj *) lst);
    lsp_obj_pool_release_obj((lsp_obj *) lst);

    return (lsp_obj *) new_lst;
}

lsp_obj *builtin_car(lsp_list *argl)
{
    REQUIRES_N_ARGS("car", argl, 1);
    lsp_list *lst = (lsp_list *) lsp_list_get_eval(argl, 1);
    if (!lst || lsp_list_len(lst) == 0) {
        lsp_obj_destroy((lsp_obj *) lst);
        lsp_obj_pool_release_obj((lsp_obj *) lst);
        return lsp_obj_new(OBJ_LIST);
    }
    assert(lst && lst->type == OBJ_LIST);
    lsp_obj *fst = lsp_list_get(lst, 0);
    assert(fst);
    lsp_obj *clone = lsp_obj_clone(fst);
    assert(clone);
    lsp_obj_destroy((lsp_obj *) lst);
    lsp_obj_pool_release_obj((lsp_obj *) lst);
    return clone;
}

lsp_obj *builtin_cdr(lsp_list *argl)
{
    REQUIRES_N_ARGS("cdr", argl, 1);
    lsp_list *lst = (lsp_list *) lsp_list_get_eval(argl, 1);
    if (!lst || lsp_list_len(lst) == 0) {
        lsp_obj_destroy((lsp_obj *) lst);
        lsp_obj_pool_release_obj((lsp_obj *) lst);
        return lsp_obj_new(OBJ_LIST);
    }
    assert(lst && lst->type && OBJ_LIST);
    lsp_list *rest = lsp_list_after(lst, 1);
    lsp_obj_destroy((lsp_obj *) lst);
    lsp_obj_pool_release_obj((lsp_obj *) lst);
    return (lsp_obj *) rest;
}


// NOTE: does not support merging of lists
// assumes left is object, and right is list.
lsp_obj *builtin_append(lsp_list *argl)
{
    REQUIRES_N_ARGS("append", argl, 2);
    lsp_obj *obj = lsp_list_get_eval(argl, 1);
    assert(obj);
    lsp_list *lst = (lsp_list *) lsp_list_get_eval(argl, 2);
    assert(lst && lst->type == OBJ_LIST);
    lsp_list_push(lst, (lsp_obj *) obj);
    return (lsp_obj *) lst;
}

lsp_obj *builtin_reverse(lsp_list *argl)
{
    REQUIRES_N_ARGS("reverse", argl, 1);
    lsp_list *lst = (lsp_list *) lsp_list_get_eval(argl, 1);
    assert(lst && lst->type == OBJ_LIST);

    lsp_list *rev = (lsp_list *) lsp_obj_new(OBJ_LIST);
    assert(rev);

    size_t len = lsp_list_len(lst);
    for (size_t i = 0; i < len; i++) {
        // TODO: change to pop
        //lsp_obj *obj = lsp_list_get(lst, len-i-1);
        lsp_obj *obj = vector_pop_lsp_obj_ptr(&lst->vec);
        assert(!lsp_list_push(rev, obj));
    }
    lsp_obj_destroy((lsp_obj *) lst);
    lsp_obj_pool_release_obj((lsp_obj *) lst);

    return (lsp_obj *) rev;
}

lsp_obj *builtin_list(lsp_list *argl)
{
    REQUIRES_ATLEAST_N_ARGS("list", argl, 1);

    lsp_list *lst = (lsp_list *) lsp_obj_new(OBJ_LIST);
    assert(lst);

    size_t len = lsp_list_len(argl);
    for (size_t i = 1; i < len; i++) {
        lsp_obj *obj = (lsp_obj *) lsp_list_get_eval(argl, i);
        assert(obj);
        assert(!lsp_list_push(lst, obj));
    }

    return (lsp_obj *) lst;
}


lsp_obj *builtin_loadfile(lsp_list *argl)
{
    REQUIRES_N_ARGS("loadfile", argl, 1);
    lsp_str *str = (lsp_str *) lsp_list_get_eval(argl, 1);
    assert(str && str->type == OBJ_STRING);

    const char *cstr = str->ptr;
    assert(cstr);
    FILE *fp = fopen(cstr, "r");
    if (!fp) {
        fprintf(stderr, "Runtime error: unable to open file `%s`!\n", cstr);
    } else {
        int ret = load_file(fp, false, false, true); // set the last true to false when not in repl
        if (ret) {
            fprintf(stderr, "Runtime error: failed to load file `%s`!\n", cstr);
        }
        fclose(fp);
    }

    lsp_obj_destroy((lsp_obj *) str);
    lsp_obj_pool_release_obj((lsp_obj *) str);

    return lsp_obj_new(OBJ_GENERIC);
}
