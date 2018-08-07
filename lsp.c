#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <stdint.h>

#include "vector.h"
#include "interp.h"
#include "token.h"
#include "utils.h"

// reads until the count of START_LIST tokens == the count of END_LIST tokens
// or, if count START_LIST tokens == 0, then the first atom.
static int repl_read_next(FILE *fp, char **buf, size_t *bsize,
                          vector_token *tokens, bool interactive)
{
    // TODO: simplify this function
    assert(fp);

    size_t last_i = 0;
    ssize_t line_len = 0;
    int ret = 0;
    size_t lists = 0;
    size_t tokens_start = 0;

    bool done = false;

    if (interactive) {
        fprintf(stdout, "REPL: ");
    }
    while (!done) {
        if (last_i >= line_len) {
            if (lists > 0 && interactive) {
                fprintf(stdout, "> ");
            }
            line_len = getline(buf, bsize, fp);
            if (line_len < 0) {
                done = true;
                ret = 1;
                break;
            }
            last_i = 0;
            //fprintf(stderr, "GETLINE: `%s`\n", *buf);
        }

        //fprintf(stderr, "BUF: `%s`\n", *buf);
        // update the last pointer using the saved offset
        const char *start = (*buf) + last_i;
        const char *last = start;

        // try tokenize_str__ with the tokens
        int r = tokenize_str_r(start, tokens, &last);
        if (r == TOKENIZE_STR_DONE) {
            done = true;
            break;
        }
        //if (!r) {
        //    done = true;
        //    ret = r;
        //    break;
        //}

        // since tokenize_str__ takes last as a pointer, and
        // the pointer may become invalidated on reallocation
        // store the index offset.
        last_i = last - *buf;
        //fprintf(stderr, "tokens_start: %lu, lists: %lu, r: %d, last_i: %lu\n", tokens_start, lists, r, last_i);

        //fprintf(stderr, "TOKENS: ");
        for (size_t i = tokens_start; i < tokens->len; i++) {
            token token = vector_get_token(tokens, i);
            if (token.type == T_LIST_START) {
                lists++;
            } else if (token.type == T_LIST_END) {
                lists--;
            }
            //token_print(&token);
        }
        tokens_start = tokens->len;

        if (lists == 0) {
            break;
        }
    }
    return ret;
}

static void create_and_execute_ast(vector_token *tokens, bool print_rlist)
{
    // create ast
    lsp_list *ast = create_ast(tokens);
    assert(ast);

    // execute
    lsp_list *rlst = execute_ast(ast);
    assert(rlst);
    if (print_rlist) {
        for (size_t i = 0; i < rlst->vec.len; i++) {
            lsp_obj *obj = vector_get_lsp_obj_ptr(&rlst->vec, i);
            if (obj) {
                lsp_obj_print_repr(obj);
            } else {
                printf("#<generic %p>\n", obj);
            }
        }
    }

    lsp_obj_destroy((lsp_obj *) rlst);
    free(rlst);
    lsp_obj_destroy((lsp_obj *) ast);
    free(ast);
}

static int repl_start(bool print_tokens)
{
    vector_token tokens;
    assert(!vector_init_token(&tokens));

    char *s = NULL;
    size_t ss = 0;

    while (!repl_read_next(stdin, &s, &ss, &tokens, true)) {
        if (!print_tokens) {
            create_and_execute_ast(&tokens, true);
        }

        for (size_t i = 0; i < tokens.len; i++) {
            token token = vector_get_token(&tokens, i);
            if (print_tokens) {
                token_print(&token);
            }
            token_destroy(&token);
        }
        tokens.len = 0;
    }

    // cleanup
    free(s);

    // make sure all tokens are destroyed
    while (tokens.len > 0) {
        token token = vector_pop_token(&tokens);
        if (tokens.error) {
            break;
        }
        token_destroy(&token);
    }
    assert(!vector_destroy_token(&tokens));
    return 0;
}

static int execute_file(FILE *fp)
{
    vector_token tokens;
    assert(!vector_init_token(&tokens));

    char *s = NULL;
    size_t ss = 0;

    while (!repl_read_next(fp, &s, &ss, &tokens, false)) {
        create_and_execute_ast(&tokens, false);

        for (size_t i = 0; i < tokens.len; i++) {
            token token = vector_get_token(&tokens, i);
            //token_print(&token);
            token_destroy(&token);
        }
        tokens.len = 0;
    }

    // cleanup
    free(s);

    // make sure all tokens are destroyed
    while (tokens.len > 0) {
        token token = vector_pop_token(&tokens);
        if (tokens.error) {
            break;
        }
        token_destroy(&token);
    }
    assert(!vector_destroy_token(&tokens));
    return 0;
}

int main(int argc, const char *argv[])
{
    bool use_repl = false;
    bool print_tokens = false;
    if (argc < 2) {
        use_repl = true;
    } else if (argv[1][0] == '-' && argv[1][1] == 't') {
        use_repl = true;
        print_tokens = true;
    }

    if (use_repl) {
        return repl_start(print_tokens);
    } else {
        for (int i = 1; i < argc; i++) {
            //fprintf(stderr, "Executing: %s\n", argv[i]);
            FILE *fp = fopen(argv[i], "r");
            if (!fp) {
                perror("fopen");
                exit(1);
            }
            int ret = execute_file(fp);
            fclose(fp);
            if (ret) {
                return ret;
            }
        }
    }
}
