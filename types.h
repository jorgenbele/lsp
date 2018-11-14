#ifndef __TYPES_H_
#define __TYPES_H_

#include <stdint.h>
#include <stdbool.h>

#include "vector.h"

/*
 * Defines the lsp datatypes.

 * NOTE: All lsp datatypes should be
 * stored in the lsp stack.
 */

enum lsp_obj_type {OBJ_GENERIC, OBJ_LIST, OBJ_INT, OBJ_FLOAT, OBJ_STRING, OBJ_SYMBOL};
typedef enum lsp_obj_type lsp_obj_type;

extern const char *obj_type_str[];
extern const char *obj_type_str_short[];

/*
 * type is an enum which determines what type the object
 * should be treated as.
 *
 * size is an unsigned 64-bit integer representing the
 * amount of allocated bytes set aside at 'ptr'.
 *
 * ptr is a pointer to the objects data.
 */

#define LSP_OBJ_STRUCT_NO_UNION                 \
    lsp_obj_type type;                          \
    uint64_t size

#define LSP_OBJ_STRUCT_UNION                    \
    void *ptr;                                  \
    int64_t integer;                            \
    double flt

#define LSP_OBJ_STRUCT \
    LSP_OBJ_STRUCT_NO_UNION; \
    union { LSP_OBJ_STRUCT_UNION; }

struct lsp_obj {
    LSP_OBJ_STRUCT;
};
typedef struct lsp_obj lsp_obj;

// Internal representation of a list is a vector
// of pointers to objects with type vector_lsp_obj_ptr.
typedef lsp_obj* lsp_obj_ptr;
DEF_VECTOR_HEADER(lsp_obj_ptr, lsp_obj_ptr)

struct lsp_list {
    LSP_OBJ_STRUCT;
    vector_lsp_obj_ptr vec; // element vector
};
typedef struct lsp_list lsp_list;

struct lsp_str {
    LSP_OBJ_STRUCT;
    uint64_t len;
};
typedef struct lsp_str lsp_str;

struct lsp_symbol {
    LSP_OBJ_STRUCT;
    char *symb;      // const
    size_t symb_len; // const
    bool func;       // is this a function?
    lsp_obj *val;    // value
};
typedef struct lsp_symbol lsp_symbol;

// store the largest possible object size size
// this value is used when allocating new "generic" objects
#define MAX_LSP_OBJ_SIZE (sizeof(lsp_symbol) > sizeof(lsp_list) ? sizeof(lsp_symbol) : sizeof(lsp_list))

int lsp_obj_init_w(lsp_obj *obj, lsp_obj_type type, void *data, size_t size);
int lsp_obj_init(lsp_obj *obj, lsp_obj_type type);
int lsp_obj_destroy(lsp_obj *obj);
bool lsp_obj_is_true(lsp_obj *obj);
int lsp_obj_cmp(lsp_obj *obj1, lsp_obj *obj2);

lsp_obj *lsp_obj_new_w(lsp_obj_type type, void *data, size_t size);
lsp_obj *lsp_obj_new(lsp_obj_type type);
lsp_obj *lsp_obj_clone(const lsp_obj *src);

int lsp_obj_print(lsp_obj *obj);
int lsp_obj_print_repr(lsp_obj *obj);
lsp_str *lsp_obj_repr(lsp_obj *obj);
int lsp_obj_repr_str(lsp_obj *obj, char **out, size_t *size);
int lsp_obj_to_str(lsp_obj *obj, char **out, size_t *size);

void lsp_obj_pool_print_stats();
int lsp_obj_pool_init();
int lsp_obj_pool_destroy();
lsp_obj *lsp_obj_pool_take_obj();
int lsp_obj_pool_release_obj(lsp_obj *obj);

int lsp_str_init_w(lsp_str *str, const void *data, size_t size);
int lsp_str_init(lsp_str *str);
int lsp_str_destroy(lsp_str *str);
int lsp_str_cat_n(lsp_str *lstr, const char *str, size_t str_len);

lsp_obj *lsp_obj_eval(lsp_obj *obj);
lsp_obj *lsp_symbol_eval(const lsp_symbol *symb);

int lsp_list_push(lsp_list *lst, lsp_obj *obj);
size_t lsp_list_len(lsp_list *lst);
int lsp_list_error(lsp_list *lst);
lsp_obj *lsp_list_get(lsp_list *lst, size_t i);
lsp_obj *lsp_list_get_eval(lsp_list *lst, size_t i);
lsp_list *lsp_list_after(lsp_list *lst, size_t i);
lsp_list *lsp_list_first(lsp_list *lst);
lsp_list *lsp_list_last(lsp_list *lst);

#endif // __TYPES_H_
