#ifndef __INTERP_H_
#define __INTERP_H_

#include <time.h>

#include "types.h"
#include "token.h"


enum e_interp_state {NORMAL, IN_LIST};
struct interp_state {
    enum e_interp_state state;
    size_t stack_start;
};

typedef lsp_list* lsp_list_ptr;
DEF_VECTOR_HEADER(lsp_list_ptr, lsp_list_ptr)

// DEPRECATED
typedef struct interp_state interp_state;
DEF_VECTOR_HEADER(interp_state, interp_state)

// Holds information which needs to be stored
// between calls to ast_execute.
struct interp_ctx {
    //lsp_list *symbols; // global symbols (set by defvar, defun, etc...)
    vector_lsp_list_ptr symbols_stack;
    size_t n_obj_heap_new;
    size_t n_obj_destroy;
    size_t n_obj_eval;
    struct timespec start;
};
typedef struct interp_ctx interp_ctx;

extern interp_ctx global_interp_ctx;


int interp_init();
int interp_destroy();

lsp_list *ast_build(vector_token *tokens);
lsp_list *ast_execute(lsp_list *ast);

int interp_ctx_init(interp_ctx *ctx);
int interp_ctx_destroy(interp_ctx *ctx);

lsp_obj *list_evaluate(lsp_list *lst);
lsp_obj *execute_defun_func(lsp_symbol *symb, lsp_list *argl);

// DEPRECATED
int exec_tokens(vector_token *tokens);
vector_lsp_obj_ptr *exec_tokens_(vector_token *tokens);


#endif // __INTERP_H_
