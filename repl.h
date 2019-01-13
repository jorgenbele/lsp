#ifndef __REPL_H_
#define __REPL_H_

#include <stdio.h>

#include "token.h"

#define REPL_F_PRINT_AST    0x01
#define REPL_F_PRINT_TOKENS 0x02
#define REPL_F_PRINT_TIME   0x04
#define REPL_F_VERBOSE      0x08
#define REPL_F_PRINT_RLIST  0x10

int repl_read_next(FILE *fp, char **buf, size_t *bsize,
                          vector_token *tokens, bool interactive);

void build_and_execute_ast(vector_token *tokens, unsigned int flags);
int repl_start(unsigned int flags);
int load_file(FILE *fp, unsigned int flags);

#endif // __REPL_H_
