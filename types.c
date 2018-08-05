#include <stdio.h>
#include <stdbool.h>
#include <assert.h>

#include "types.h"
#include "utils.h"

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

int lsp_obj_init_w(lsp_obj *obj, lsp_obj_type type, void *data, size_t size)
{
    switch (type) {
        case OBJ_STRING: {
            // NOTE size is actually the length of the string.
            lsp_str *str = (lsp_str*) obj;
            lsp_str_init_w(str, data, size);
            break;
        }

        case OBJ_INT:
            obj->type = type;
            obj->size = 0;
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
            // Destroy the vector.
            lsp_list *lst = (lsp_list *) obj;
            assert(!vector_destroy_lsp_obj_ptr(&lst->vec));
            break;
        }

        case OBJ_SYMBOL: {
            lsp_symbol *symb = (lsp_symbol *) obj;
            // TODO: free data?
            free(symb->symb);
            symb->symb = NULL;
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

static int repr_(lsp_obj *obj, char **out, size_t *size, bool repr)
{
    if (repr) {
        const char *short_name = obj_type_str_short[obj->type];
        alloc_strcatf(out, size, "%s:", short_name);
    }

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
            break;

        case OBJ_GENERIC:
            alloc_strcatf(out, size, "%x:size:%lu", obj->ptr, obj->size);
            break;
                
        case OBJ_LIST: {
            alloc_strcatf(out, size, "(");

            lsp_list *lst = (lsp_list *) obj;
            for (size_t k = 0; k < lst->vec.len; k++) {
                lsp_obj *o = vector_get_lsp_obj_ptr(&lst->vec, k);
                if (lsp_obj_repr_str(o, out, size)) {
                    return 1;
                }
                if (k + 1 < lst->vec.len) {
                    alloc_strcatf(out, size, " ");
                } 
            }
            alloc_strcatf(out, size, ")");
            break;
        }
    }

    return 0;
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
 * Generic
 */
