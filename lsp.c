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

enum {PRINT_AST=0x01, PRINT_TOKENS=0x02, PRINT_TIME=0x04, PRINT_RLIST=0x08, USE_REPL=0x16};

int main(int argc, const char *argv[])
{
    int ret = 0;

    bool use_repl = false;
    bool print_ast = false;
    bool print_tokens = false;
    bool print_time = false;

    int start_arg = 1;

    const char **load_files = xcalloc(argc, sizeof(*load_files));
    const char **load_files_ptr = load_files;

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
                    case 'l':
                        if (i+1 >= argc) {
                            fprintf(stderr, "Flag -l is missing <file>\n");
                            ret = 1;
                            goto parse_args_failed;
                        }
                        *(load_files_ptr++) = argv[i+1];
                        i++;
                        start_arg++;
                        goto inner_loop_end;
                        break;
                    default:
                        fprintf(stderr, "Unknown flag: '%c'\n", argv[i][k]);
                        ret = 1;
                        break;
                }
            }
        inner_loop_end:
            start_arg++;
        } else {
            break;
        }
    }

    if (argc - start_arg < 1) {
        use_repl = true;
    }

    lsp_obj_pool_init();
    assert(!interp_init());

    load_files_ptr = load_files;
    while (*load_files_ptr) {
        FILE *fp = fopen(*load_files_ptr, "r");
        if (!fp) {
            perror("fopen");
            ret = 1;
            goto load_files_failed;
        }
        int r = load_file(fp, print_ast, print_tokens, print_time);
        fclose(fp);
        if (r) {
            ret = r;
            goto load_files_failed;
        }
        fprintf(stderr, "loaded file: `%s`\n", *load_files_ptr);
        load_files_ptr++;
    }

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
            //int r = execute_file(fp, print_ast, print_tokens, print_time);
            int r = load_file(fp, print_ast, print_tokens, print_time);
            fclose(fp);
            if (r) {
                ret = r;
                break;
            }
        }
    }

    lsp_obj_pool_print_stats();
    lsp_obj_pool_destroy();

load_files_failed:
    assert(!interp_destroy());

parse_args_failed:
    free(load_files);

    return ret;
}
