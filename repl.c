#define _POSIX_C_SOURCE 200809L

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <stdint.h>
#include <time.h>
#include <stddef.h>
#include <unistd.h>

#include "vector.h"
#include "interp.h"
#include "token.h"
#include "utils.h"


#include "repl.h"

// reads until the count of START_LIST tokens == the count of END_LIST tokens
// or, if count START_LIST tokens == 0, then the first atom.
int repl_read_next(FILE *fp, char **buf, size_t *bsize,
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

    tokenizer_ctx tokenizer_ctx;
    memset(&tokenizer_ctx, 0, sizeof(tokenizer_ctx));

    if (interactive) {
        fprintf(stdout, "REPL: ");
        fflush(stdout);
    }
    while (!done) {
        // line_len is never negative in this state
        if (last_i >= (size_t)line_len) {
            if (lists > 0 && interactive) {
                fprintf(stdout, "> ");
                fflush(stdout);
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
        //const char *last = start;
        tokenizer_ctx.last = start;

        // try tokenize_str__ with the tokens
        int r = tokenize_str_r(start, tokens, &tokenizer_ctx);
        if (r == TOKENIZE_STR_DONE) {
            done = true;
            break;
//        } else if (r) {
//            // error
//            return r;
        }

        // since tokenize_str__ takes last as a pointer, and
        // the pointer may become invalidated on reallocation
        // store the index offset.
        last_i = tokenizer_ctx.last - *buf;
        //fprintf(stderr, "tokens_start: %lu, lists: %lu, r: %d, last_i: %lu\n", tokens_start, lists, r, last_i);

        //fprintf(stderr, "TOKENIZER STATE: %s\n", tokenizer_state_str[vector_peek_tokenizer_state(&tokenizer_ctx.states).type]);

        for (size_t i = tokens_start; i < tokens->len; i++) {
            token token = vector_get_token(tokens, i);
            if (token.type == T_LIST_START) {
                lists++;
            } else if (token.type == T_LIST_END) {
                lists--;
            }
        }
        tokens_start = tokens->len;

        if (lists == 0) {
            break;
        }
    }
    assert(!vector_destroy_tokenizer_state(&tokenizer_ctx.states));
    return ret;
}

void build_and_execute_ast(vector_token *tokens, unsigned int flags)
{
    // create ast
    lsp_list *ast = ast_build(tokens);
    assert(ast);
    if (flags & REPL_F_PRINT_AST) {
        lsp_obj_print_repr((lsp_obj *) ast);
    }

    // execute
    lsp_list *rlst = ast_execute(ast);
    assert(rlst);
    if (flags & REPL_F_PRINT_RLIST) {
        for (size_t i = 0; i < rlst->vec.len; i++) {
            lsp_obj *obj = vector_get_lsp_obj_ptr(&rlst->vec, i);
            if (obj) {
                lsp_obj_print_repr(obj);
            } else {
                printf("#<generic %p>\n", (void *) obj);
            }
        }
    }

    lsp_obj_destroy((lsp_obj *) rlst);
    //free(rlst);
    lsp_obj_pool_release_obj((lsp_obj *) rlst);
    lsp_obj_destroy((lsp_obj *) ast);
    //free(ast);
    lsp_obj_pool_release_obj((lsp_obj *) ast);
}

#define START_TIME_TAKING_BLOCK(enabled)                \
    do {                                                \
    struct timespec start;                              \
    assert(!clock_gettime(CLOCK_MONOTONIC, &start));

#define END_TIME_TAKING_BLOCK(enabled)                          \
    struct timespec end;                                        \
    assert(!clock_gettime(CLOCK_MONOTONIC, &end));              \
    if (enabled) {                                              \
        int64_t diff_s = end.tv_sec - start.tv_sec;             \
        int64_t diff_ns = end.tv_nsec - start.tv_nsec;          \
        int64_t diff_ms = diff_ns/1000000;                      \
        fprintf(stderr, "Executed in %ld s %ld ns (%ld ms)\n",  \
                diff_s, diff_ns, diff_ms);                      \
    }                                                           \
    } while (0);

int repl_start(unsigned int flags)
{
    vector_token tokens;
    assert(!vector_init_token(&tokens));

    char *s = NULL;
    size_t ss = 0;

    while (!repl_read_next(stdin, &s, &ss, &tokens, true)) {
        //if (!(flags & REPL_F_PRINT_TOKENS) || (flags & REPL_F_PRINT_AST)) {
        //    lsp_obj_pool_print_stats();
        //}
        if (!(flags & REPL_F_PRINT_TOKENS)) {
            START_TIME_TAKING_BLOCK(flags & REPL_F_PRINT_TIME)
            build_and_execute_ast(&tokens, flags | REPL_F_PRINT_RLIST);
            END_TIME_TAKING_BLOCK(flags & REPL_F_PRINT_TIME)
        }

        for (size_t i = 0; i < tokens.len; i++) {
            token token = vector_get_token(&tokens, i);
            if (flags & REPL_F_PRINT_TOKENS) {
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

int load_file(FILE *fp, unsigned int flags)
{
    vector_token tokens;
    assert(!vector_init_token(&tokens));

    char *s = NULL;
    size_t ss = 0;

    while (!repl_read_next(fp, &s, &ss, &tokens, false)) {
        if (flags & REPL_F_PRINT_TOKENS) {
            for (size_t i = 0; i < tokens.len; i++) {
                token token = vector_get_token(&tokens, i);
                token_print(&token);
            }
        }

        START_TIME_TAKING_BLOCK(flags & REPL_F_PRINT_TIME)
        build_and_execute_ast(&tokens, flags);
        END_TIME_TAKING_BLOCK(flags & REPL_F_PRINT_TIME)

        for (size_t i = 0; i < tokens.len; i++) {
            token token = vector_get_token(&tokens, i);
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
