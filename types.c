#include <stdio.h>
#include <stdbool.h>
#include <assert.h>

#include "types.h"
#include "utils.h"
#include "builtins.h" // circular dependency ???
#include "interp.h"

const char *obj_type_str[] = {
    "OBJ_GENERIC", "OBJ_LIST", "OBJ_INT", "OBJ_FLOAT", "OBJ_STRING", "OBJ_SYMBOL"
};

const char *obj_type_str_short[] = {
    "#grc", "#list", "#int", "#float", "#str", "#sym"
};

// Functions for working with the lsp_obj_ptr list
DEF_VECTOR_FUNCS(lsp_obj_ptr, lsp_obj_ptr, NULL);

/*
 * Object
 */
enum lsp_obj_err {LSP_OBJ_ERR_MALLOC};

bool lsp_obj_is_true(lsp_obj *obj)
{
    switch (obj->type) {
        case OBJ_STRING: {
            // is true if not empty
            // NOTE size is actually the length of the string.
            lsp_str *str = (lsp_str*) obj;
            return str->len > 0;
        }

        case OBJ_INT:
            return obj->integer;

        case OBJ_FLOAT:
            return obj->flt;

        case OBJ_SYMBOL: {
            // is true if it exists.
            lsp_symbol *symb = (lsp_symbol *) obj;
            return (builtin_get_func(symb->symb));
        }

        case OBJ_GENERIC:
            return obj->ptr;

        case OBJ_LIST: {
            // is true if not empty
            lsp_list *lst = (lsp_list *) obj;
            return lst->vec.len > 0;
        }
    }
}

// TODO refactor
lsp_obj *lsp_obj_clone(const lsp_obj *src)
{
    lsp_obj *robj = NULL;
    switch (src->type) {
        case OBJ_STRING: {
            // NOTE size is actually the length of the string.
            const lsp_str *str = (const lsp_str *) src;
            robj = lsp_obj_new_w(str->type, str->ptr, str->len); // NOTE len
            break;
        }

        case OBJ_INT: {
            robj = lsp_obj_new_w(src->type, src->ptr, src->size);
            robj->integer = src->integer;
            break;
        }

        case OBJ_FLOAT: {
            robj = lsp_obj_new_w(src->type, src->ptr, src->size);
            robj->flt = src->flt;
            break;
        }

        case OBJ_LIST: {
            // NOTE not const since vector_* takes non-const.
            lsp_list *src_lst = (lsp_list *) src;
            robj = lsp_obj_new(src_lst->type);
            lsp_list *lst = (lsp_list *) robj;

            // recursively clone all objects
            for (size_t i = 0; i < src_lst->vec.len; i++) {
                const lsp_obj *src_o = vector_get_lsp_obj_ptr(&src_lst->vec, i);
                assert(src_o);
                lsp_obj *ret_o = lsp_obj_clone(src_o);
                assert(ret_o);
                assert(!vector_push_lsp_obj_ptr(&lst->vec, ret_o));
            }
            break;
        }

        case OBJ_SYMBOL: {
            const lsp_symbol *src_symb = (const lsp_symbol *) src;
            robj = lsp_obj_new(src_symb->type);
            lsp_symbol *symb = (lsp_symbol *) robj;
            assert(symb);
            symb->symb_len = src_symb->symb_len;
            symb->symb = xstrdupn(src_symb->symb, src_symb->symb_len);
            assert(symb->symb);
            break;
        }

        case OBJ_GENERIC:
            robj = lsp_obj_new_w(src->type, src->ptr, src->size);
            break;
    }
    return robj;
}

int lsp_obj_init_w(lsp_obj *obj, lsp_obj_type type, void *data, size_t size)
{
    static size_t inited = 0;
    inited++;
    //fprintf(stderr, "lsp_obj_inited: %lu\n", inited);
    switch (type) {
        case OBJ_STRING: {
            // NOTE size is actually the length of the string.
            lsp_str *str = (lsp_str*) obj;
            lsp_str_init_w(str, data, size);
            break;
        }

        case OBJ_INT:
            obj->type = type;
            obj->size = 0; // not a pointer
            obj->integer = 0;
            break;

        case OBJ_FLOAT:
            obj->type = type;
            obj->size = 0;
            obj->flt = 0.0;
            break;

        case OBJ_LIST: {
            lsp_list *lst = (lsp_list *) obj;
            lst->type = type;
            lst->size = sizeof(lst->vec);
            assert(!vector_init_lsp_obj_ptr(&lst->vec));
            break;
        }

        case OBJ_SYMBOL: {
            lsp_symbol *symb = (lsp_symbol *) obj;
            symb->type = type;
            symb->ptr = data;
            symb->size = sizeof(obj->size);
            break;
        }

        case OBJ_GENERIC:
            obj->size = size;
            obj->ptr = data;
            obj->type = type;
            assert(type == OBJ_GENERIC);
            break;
    }
    return 0;
}

int lsp_obj_init(lsp_obj *obj, lsp_obj_type type)
{
    return lsp_obj_init_w(obj, type, NULL, 0);
}

int lsp_obj_destroy(lsp_obj *obj)
{
    static size_t destroyed = 0;
    destroyed++;
    //fprintf(stderr, "lsp_obj_destroyed: %lu\n", destroyed);
    switch (obj->type) {
        case OBJ_INT:
        case OBJ_FLOAT:
            // Nothing to destroy.
            break;

        case OBJ_STRING:
            free(obj->ptr);
            obj->ptr = NULL;
            break;

        case OBJ_LIST: {
            // The list can be recursive. Therefore
            // it it required to destroy all
            // items of the list
            // Destroy the vector.
            lsp_list *lst = (lsp_list *) obj;
            while (lst->vec.len > 0 && !lst->vec.error) {
                lsp_obj *obj = vector_pop_lsp_obj_ptr(&lst->vec);
                if (!obj) {
                    break;
                }
                // NOTE: Recursive call
                assert(!lsp_obj_destroy(obj));
                free(obj);
            }
            assert(!vector_destroy_lsp_obj_ptr(&lst->vec));
            break;
        }

        case OBJ_SYMBOL: {
            lsp_symbol *symb = (lsp_symbol *) obj;
            // TODO: free data?
            free(symb->symb);
            symb->symb = NULL;
            if (symb->val) {
                assert(!lsp_obj_destroy(symb->val));
                free(symb->val);
                symb->val = NULL;
            }
            break;
        }

        case OBJ_GENERIC:
            // ...
            free(obj->ptr);
            obj->ptr = NULL;
            break;
        
    }
    return 0;
}

lsp_obj *lsp_obj_new_w(lsp_obj_type type, void *data, size_t size)
{
    size_t obj_size = 0;
    switch (type) {
        case OBJ_STRING:
            obj_size = sizeof(lsp_str);
            break;

        case OBJ_INT:
        case OBJ_GENERIC:
        case OBJ_FLOAT:
            obj_size = sizeof(lsp_obj);
            break;

        case OBJ_LIST:
            obj_size = sizeof(lsp_list);
            break;

        case OBJ_SYMBOL:
            obj_size = sizeof(lsp_symbol);
            break;

        default:
            fprintf(stderr, "Runtime error: unable to create new type"
                            ", unknown type: %u\n", type);
            exit(1);
            break;
    }
    assert(obj_size > 0);
    lsp_obj *obj = xcalloc(1, obj_size);
    assert(obj);
    assert(!lsp_obj_init_w(obj, type, data, size));
    return obj;
}

lsp_obj *lsp_obj_new(lsp_obj_type type)
{
    return lsp_obj_new_w(type, NULL, 0);
}

lsp_obj *lsp_obj_eval(lsp_obj *obj)
{
    if (obj->type == OBJ_LIST) {
        return list_evaluate((lsp_list *) obj);
    } else if (obj->type == OBJ_SYMBOL) {
        return lsp_symbol_eval((lsp_symbol *) obj);
    }
    return lsp_obj_clone(obj);
}


lsp_obj *lsp_symbol_eval(const lsp_symbol *symb)
{
// check if it exists in the symbols list, and if so return
// (a clone of) the val it contains.
    size_t len = lsp_list_len(&global_interp_ctx.symbols);
    for (size_t i = 0; i < len; i++) {
        const lsp_obj *e_obj = lsp_list_get(&global_interp_ctx.symbols, i);
        assert(e_obj->type == OBJ_SYMBOL);
        const lsp_symbol *e_symb = (lsp_symbol *) e_obj;

        if (e_symb->symb_len == symb->symb_len
            && !strncmp(e_symb->symb, symb->symb, e_symb->symb_len)) {
            if (e_symb->val->type == OBJ_SYMBOL) {
                // recurse until no symbol is found
                // or it is itself
                return lsp_symbol_eval(e_symb);
            }
            return lsp_obj_clone(e_symb->val);
        }
    }
    return lsp_obj_clone((lsp_obj *) symb); // evaluates to itself
}


static int repr_(lsp_obj *obj, char **out, size_t *size, bool repr)
{
    if (repr) {
        assert(obj);
        const char *short_name = obj_type_str_short[obj->type];
        alloc_strcatf(out, size, "%s:", short_name);
    }

    int ret = 0;
    switch (obj->type) {
        case OBJ_STRING:
            alloc_strcatf(out, size, "\"%s\"", obj->ptr);
            break;

        case OBJ_INT:
            alloc_strcatf(out, size, "%ld", obj->integer);
            break;

        case OBJ_FLOAT:
            alloc_strcatf(out, size, "%lf", obj->flt);
            break;

        case OBJ_SYMBOL:
            alloc_strcatf(out, size, "%s", ((lsp_symbol *)obj)->symb);
            if (((lsp_symbol *)obj)->val) {
                alloc_strcatf(out, size, ":");
                ret = repr_(((lsp_symbol *)obj)->val, out, size, repr);
            }
            break;

        case OBJ_GENERIC:
            alloc_strcatf(out, size, "%x:size:%lu", obj->ptr, obj->size);
            break;
                
        case OBJ_LIST: {
            alloc_strcatf(out, size, "(");

            lsp_list *lst = (lsp_list *) obj;
            assert(lst);
            size_t len = lsp_list_len(lst);
            for (size_t k = 0; k < len; k++) {
                lsp_obj *o = lsp_list_get(lst, k);
                assert(o);
                assert(!lsp_list_error(lst));
                if (repr_(o, out, size, repr)) {
                    ret = 1;
                    goto end;
                }
                //if (lsp_obj_repr_str(o, out, size)) {
                //    ret = 1;
                //    goto end;
                //}
                if (k + 1 < lst->vec.len) {
                    alloc_strcatf(out, size, " ");
                } 
            }
            alloc_strcatf(out, size, ")");
            break;
        }
    }

end:
    return ret;
}

int lsp_obj_repr_str(lsp_obj *obj, char **out, size_t *size)
{
    return repr_(obj, out, size, true);
}


int lsp_obj_print_repr(lsp_obj *obj)
{
    char *buf = NULL;
    size_t buf_s = 0;
    if (lsp_obj_repr_str(obj, &buf, &buf_s)) {
        free(buf);
        return 1;
    }

    puts(buf);
    free(buf);
    return 0;
}

int lsp_obj_print(lsp_obj *obj)
{
    switch (obj->type) {
        case OBJ_STRING:
            printf("%s", obj->ptr);
            break;
        case OBJ_INT:
            printf("%ld", obj->integer);
            break;
        case OBJ_FLOAT:
            printf("%lf", obj->flt);
            break;

        case OBJ_SYMBOL: {
            lsp_symbol *symb = (lsp_symbol *) obj;
            printf("%s", symb->symb);
            break;
        }

        case OBJ_GENERIC: {
            printf("#generic:size:%lu", obj->size);
            break;
        }
                
        case OBJ_LIST:
            putchar('(');
            // TODO:
            lsp_list *lst = (lsp_list *) obj;
            for (size_t k = 0; k < lst->vec.len; k++) {
                lsp_obj *o = vector_get_lsp_obj_ptr(&lst->vec, k);
                if (lsp_obj_print(o)) {
                    return 1;
                }
                if (k + 1 < lst->vec.len) {
                    putchar(' ');  
                } 
            }
            putchar(')');
            break;
    }
    return 0;
}

/*
 * String
 */

int lsp_str_init_w(lsp_str *str, const void *data, size_t len)
{
    str->type = OBJ_STRING;
    str->ptr = data ? xstrdupn((char *) data, len) : NULL;
    str->size = len;
    str->len = len;
    return 0; 
}

int lsp_str_init(lsp_str *str)
{
    return lsp_str_init_w(str, NULL, 0);
}

int lsp_str_destroy(lsp_str *str)
{
    free(str->ptr);
    return 0;
}

/* Returns the length of the contents in str in bytes. */
uint64_t lsp_str_len(const lsp_str *str)
{
    return str->len;
}

int lsp_str_cat_n(lsp_str *lstr, const char *str, size_t str_len)
{
    size_t total_len = lstr->len + str_len;
    if (!lstr->ptr) {
        lstr->size = total_len + 1;
        lstr->ptr = xcalloc(1, lstr->size);
    } else if (lstr->size < total_len + 1) {
        lstr->size = total_len + 1;
        lstr->ptr = xrealloc(lstr->ptr, lstr->size);
    }
    strncat(lstr->ptr, str, str_len);
    lstr->len = total_len;
    return 0;
}

/*
 * Lists
 */

size_t lsp_list_len(lsp_list *lst)
{
    return lst->vec.len;
}

int lsp_list_error(lsp_list *lst)
{
    return lst->vec.error;
}

int lsp_list_push(lsp_list *lst, lsp_obj *obj)
{
    return vector_push_lsp_obj_ptr(&lst->vec, obj);
}

lsp_obj *lsp_list_get(lsp_list *lst, size_t i)
{
    return vector_get_lsp_obj_ptr(&lst->vec, i);
}

lsp_obj *lsp_list_get_eval(lsp_list *lst, size_t i)
{
    lsp_obj *obj = lsp_list_get(lst, i);
    if (obj) {
        return lsp_obj_eval(obj);
    }
    return NULL;
}

lsp_list *lsp_list_after(lsp_list *lst, size_t i)
{
    lsp_list *after = (lsp_list *) lsp_obj_new(OBJ_LIST);
    assert(after);

    size_t lst_len = lsp_list_len(lst);
    for (size_t k = i; k < lst_len; k++) {
        const lsp_obj *obj = lsp_list_get(lst, k);
        assert(obj);
        lsp_obj *clone = lsp_obj_clone(obj);
        assert(clone);
        lsp_list_push(after, clone);
    }

    return after;
}

/*
 * Generic
 */
