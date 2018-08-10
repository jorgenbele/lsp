#ifndef __REPL_H_
#define __REPL_H_

#include <stdio.h>

#include "token.h"

int repl_read_next(FILE *fp, char **buf, size_t *bsize,
                          vector_token *tokens, bool interactive);

void build_and_execute_ast(vector_token *tokens, bool print_ast, bool print_rlist);

int repl_start(bool print_ast, bool print_tokens, bool print_time);

int load_file(FILE *fp, bool print_ast, bool print_tokens,
                        bool print_time);
int execute_file(FILE *fp, bool print_ast, bool print_tokens,
                        bool print_time);


#endif // __REPL_H_
