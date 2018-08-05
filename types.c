#include <stdio.h>
#include <stdbool.h>
#include <assert.h>

#include "types.h"

const char *obj_type_str[] = {
    "OBJ_GENERIC", "OBJ_LIST", "OBJ_INT", "OBJ_FLOAT", "OBJ_STRING", "OBJ_SYMBOL"
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
            obj->size = sizeof(obj->integer);
            obj->integer = 0;
            break;

        case OBJ_FLOAT:
            obj->type = type;
            obj->size = sizeof(obj->flt);
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

        case OBJ_GENERIC:
        default:
            // ...
            free(obj->ptr);
            obj->ptr = NULL;
            break;
        
    }
    return 0;
}

/*
 * String
 */

int lsp_str_init_w(lsp_str *str, void *data, size_t len)
{
    str->type = OBJ_STRING;
    str->ptr = data;
    str->size = len;
    str->len = len;
    return 0; 
}

/* Returns the length of the contents in str in bytes. */
uint64_t lsp_str_len(const lsp_str *str)
{
    return str->len;
}

/*
 * Generic
 */
