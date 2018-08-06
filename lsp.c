#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "vector.h"
#include "interp.h"
#include "token.h"

int main(int argc, const char *argv[])
{
    vector_token tokens;
    assert(!vector_init_token(&tokens));

    //assert(!tokenize_str("( ;; this is a test comment test  \n )", &tokens));
    //assert(!tokenize_str("\"test string with escaped \\\\ escapes \\\"\"", &tokens));
    //assert(!tokenize_str("\"test string with escapes \\\"\"", &tokens));
    //assert(!tokenize_str("\"testing \\\" test\"  ", &tokens));
    //assert(!tokenize_str("(+ 123)", &tokens));
    //assert(!tokenize_str("(\"test\")", &tokens));

    char *s = NULL;
    size_t ss = 0;
    while (getline(&s, &ss, stdin) > 0) {
        assert(!tokenize_str(s, &tokens));
        for (size_t i = 0; i < tokens.len; i++) {
            token token = vector_get_token(&tokens, i);
            token_print(&token);
        }

        // execute interpreter
        //assert(!exec_tokens(&tokens));

        // create ast
        lsp_list *ast = create_ast(&tokens);
        assert(ast);

        fprintf(stdout, "\n\n===== AST =====\n");
        for (size_t i = 0; i < ast->vec.len; i++) {
            lsp_obj *obj = vector_get_lsp_obj_ptr(&ast->vec, i);
            assert(obj);
            lsp_obj_print_repr(obj);
        }
        fprintf(stdout, "===============\n");

        lsp_obj_destroy((lsp_obj *) ast);
        free(ast);

        for (size_t i = 0; i < tokens.len; i++) {
            token token = vector_get_token(&tokens, i);
            token_free(&token);
        }
        tokens.len = 0; // warning
    }

    free(s);


    assert(!vector_destroy_token(&tokens));
    return 0;
}
