#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <stdint.h>
#include <time.h>

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

        // since tokenize_str__ takes last as a pointer, and
        // the pointer may become invalidated on reallocation
        // store the index offset.
        last_i = last - *buf;
        //fprintf(stderr, "tokens_start: %lu, lists: %lu, r: %d, last_i: %lu\n", tokens_start, lists, r, last_i);

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
    return ret;
}

static void build_and_execute_ast(vector_token *tokens, bool print_ast, bool print_rlist)
{
    // create ast
    lsp_list *ast = ast_build(tokens);
    assert(ast);
    if (print_ast) {
        lsp_obj_print_repr((lsp_obj *) ast);
    }

    // execute
    lsp_list *rlst = ast_execute(ast);
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

static int repl_start(bool print_ast, bool print_tokens, bool print_time)
{
    vector_token tokens;
    assert(!vector_init_token(&tokens));

    char *s = NULL;
    size_t ss = 0;

    while (!repl_read_next(stdin, &s, &ss, &tokens, true)) {
        if (!print_tokens || print_ast) {
            START_TIME_TAKING_BLOCK(print_time)
            build_and_execute_ast(&tokens, print_ast, true);
            END_TIME_TAKING_BLOCK(print_time)
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

static int execute_file(FILE *fp, bool print_ast, bool print_tokens,
                        bool print_time)
{
    vector_token tokens;
    assert(!vector_init_token(&tokens));

    char *s = NULL;
    size_t ss = 0;

    while (!repl_read_next(fp, &s, &ss, &tokens, false)) {
        if (print_tokens) {
            for (size_t i = 0; i < tokens.len; i++) {
                token token = vector_get_token(&tokens, i);
                token_print(&token);
            }
        }

        START_TIME_TAKING_BLOCK(print_time);
        build_and_execute_ast(&tokens, print_ast, false);
        END_TIME_TAKING_BLOCK(print_time);

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

int main(int argc, const char *argv[])
{
    assert(!interp_init());

    bool use_repl = false;
    bool print_ast = false;
    bool print_tokens = false;
    bool print_time = false;

    int start_arg = 1;

    for (int i = 1; i < argc; i++) {
        if (argv[i][0] == '-') {
            for (int k = 1; argv[i][k]; k++) {
                switch(argv[i][k]) {
                    case 't':
                        print_tokens = true;
                        break;
                    case 'a':
                        print_ast = true;
                        break;
                    case 's':
                        print_time = true;
                        break;
                    default:
                        fprintf(stderr, "Unknown flag: '%c'\n", argv[i][1]);
                }
            }
            start_arg++;
        } else {
            break;
        }
    }

    if (argc - start_arg < 1) {
        use_repl = true;
    }

    int ret = 0;
    if (use_repl) {
        ret = repl_start(print_ast, print_tokens, print_time);
    } else {
        for (int i = start_arg; i < argc; i++) {
            FILE *fp = fopen(argv[i], "r");
            if (!fp) {
                perror("fopen");
                ret = 1;
                break;
            }
            int r = execute_file(fp, print_ast, print_tokens, print_time);
            fclose(fp);
            if (r) {
                ret = r;
                break;
            }
        }
    }

    assert(!interp_destroy());
    return ret;
}
