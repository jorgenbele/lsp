#ifndef __VECTOR_H_
#define __VECTOR_H_

#include <stdlib.h>
#include <memory.h>

enum {ERR_VECTOR_EMPTY=1};

#define DEF_VECTOR_STRUCT(name, type)           \
    struct vector_##name                        \
    {                                           \
        size_t size;                            \
        size_t len;                             \
        int error;                              \
        size_t allocs;                          \
        type *data;                             \
    };                                          \
    typedef struct vector_##name vector_##name

#define DEF_VECTOR_INIT(name, type)                 \
    int vector_init_##name(struct vector_##name *v) \
    {                                               \
        memset(v, 0, sizeof(*v));                   \
        return 0;                                   \
    }

#define DEF_VECTOR_INIT_W(name, type)                       \
    int vector_init_w_##name(struct vector_##name *v,       \
                             const type tv[], size_t ts)    \
    {                                                       \
        memset(v, 0, sizeof(*v));                           \
        v->size = ts;                                       \
        v->len = ts;                                        \
        v->allocs = 1;                                      \
        v->data = malloc(sizeof(*tv) * ts);                 \
        if (!v->data) { return 1; }                         \
        memcpy(v->data, tv, sizeof(*tv) * ts);              \
        return 0;                                           \
    }

#define DEF_VECTOR_DESTROY(name, type)                  \
    int vector_destroy_##name(struct vector_##name *v)  \
    {                                                   \
        free(v->data);                                  \
        return 0;                                       \
    }

#define VECTOR_EXTEND(type)                             \
    if (v->size < size) {                               \
    if (size && size >= v->size<<1) {                   \
        v->size = size;                                 \
    } else {                                            \
        v->size = v->size ? v->size<<1 : 2;             \
    }                                                   \
    v->data = realloc(v->data, v->size*sizeof(type));   \
    v->allocs++;                                        \
    }                                                   \
    
#define DEF_VECTOR_EXTEND(name, type)                               \
    int vector_extend_##name(struct vector_##name *v, size_t size)  \
    {                                                               \
        VECTOR_EXTEND(type);                                        \
        return !v->data;                                            \
    }

#define DEF_VECTOR_PUSH(name, type)                         \
    int vector_push_##name(struct vector_##name *v, type t) \
    {                                                       \
        if (v->len + 1 >= v->size) {                        \
            size_t size = v->len + 1;                       \
            VECTOR_EXTEND(type);                            \
            if (!v->data) { return 1; }                     \
        }                                                   \
        v->data[v->len] = t;                                \
        v->len++;                                           \
        return 0;                                           \
    }

#define DEF_VECTOR_POP(name, type, def)             \
    type vector_pop_##name(struct vector_##name *v) \
    {                                               \
        if (!v->len) {                              \
            v->error = ERR_VECTOR_EMPTY;            \
            return def;                             \
        }                                           \
        v->len--;                                   \
        return v->data[v->len];                     \
    }

#define DEF_VECTOR_PEEK(name, type, def)                \
    type vector_peek_##name(struct vector_##name *v)    \
    {                                                   \
        if (!v->len) {                                  \
            v->error = ERR_VECTOR_EMPTY;                \
            return def;                                 \
        }                                               \
        return v->data[v->len-1];                       \
    }

#define DEF_VECTOR_PEEK_PTR(name, type, def)                \
    type *vector_peek_ptr_##name(struct vector_##name *v)    \
    {                                                       \
        if (!v->len) {                                      \
            v->error = ERR_VECTOR_EMPTY;                    \
            return NULL;                                    \
        }                                                   \
        return &v->data[v->len-1];                          \
    }

#define DEF_VECTOR_GET(name, type, def)                             \
    type vector_get_##name(struct vector_##name *v, size_t i) \
    {                                                               \
        if (i + 1 > v->len) {                                       \
            v->error = ERR_VECTOR_EMPTY;                            \
            return def;                                             \
        }                                                           \
        return v->data[i];                                          \
    }

#define DEF_VECTOR_GET_PTR(name, type, def)                         \
    type *vector_get_ptr_##name(struct vector_##name *v, size_t i)   \
    {                                                               \
        if (i + 1 > v->len) {                                       \
            v->error = ERR_VECTOR_EMPTY;                            \
            return NULL;                                            \
        }                                                           \
        return &v->data[i];                                         \
    }


#define DEF_VECTOR_PROTOTYPES(name, type)                               \
    int vector_init_##name(struct vector_##name *v);                    \
    int vector_init_w_##name(struct vector_##name *v,                   \
                             const type tv[], size_t ts);               \
    int vector_destroy_##name(struct vector_##name *v);                 \
    int vector_extend_##name(struct vector_##name *v, size_t size);     \
    int vector_push_##name(struct vector_##name *v, type t);            \
    type vector_pop_##name(struct vector_##name *v);                    \
    type vector_peek_##name(struct vector_##name *v);                   \
    type *vector_peek_ptr_##name(struct vector_##name *v);              \
    type vector_get_##name(struct vector_##name *v, size_t i);          \
    type *vector_get_ptr_##name(struct vector_##name *v, size_t i)

#define DEF_VECTOR_HEADER(name, type)           \
    DEF_VECTOR_STRUCT(name, type);              \
    DEF_VECTOR_PROTOTYPES(name, type)

#define DEF_VECTOR_FUNCS(name, type, def)       \
    DEF_VECTOR_INIT(name, type);                \
    DEF_VECTOR_INIT_W(name, type);              \
    DEF_VECTOR_DESTROY(name, type)              \
        DEF_VECTOR_EXTEND(name, type);          \
    DEF_VECTOR_PUSH(name, type);                \
    DEF_VECTOR_POP(name, type, def);            \
    DEF_VECTOR_PEEK(name, type, def);           \
    DEF_VECTOR_GET(name, type, def)

#define DEF_VECTOR(name, type, def)             \
    DEF_VECTOR_HEADER(name, type);              \
    DEF_VECTOR_FUNCS(name, type, def)


#endif // __VECTOR_H_
