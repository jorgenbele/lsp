#ifndef __INTERP_H_
#define __INTERP_H_

#include "types.h"
#include "token.h"

enum e_interp_state {NORMAL, IN_LIST};
struct interp_state {
    enum e_interp_state state;
    size_t stack_start;
};

typedef lsp_list* lsp_list_ptr;
DEF_VECTOR_HEADER(lsp_list_ptr, lsp_list_ptr);

// DEPRECATED
typedef struct interp_state interp_state;
DEF_VECTOR_HEADER(interp_state, interp_state);

lsp_list *create_ast(vector_token *tokens);

// DEPRECATED
int exec_tokens(vector_token *tokens);
vector_lsp_obj_ptr *exec_tokens_(vector_token *tokens);


#endif // __INTERP_H_
