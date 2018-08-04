#ifndef _TOKEN_H_
#define _TOKEN_H_

#include <stdbool.h>

#include "vector.h"

/**
 * comment:    ';' .* NEWLINE
 * symbol:     [a-zA-Z][a-zA-Z0-9]
 * string:     '"' ([\.] | .)* '"'
 * int:        [+-]?[1-9][0-9]*
 * float:      int '.' int
 * number:     int | float
 * atom:       symbol | string | number
 * expression: atom | list
 * list:       '(' expression* ')'
 */

enum token_type {
    T_UNKNOWN,
    T_CMT_START, T_CMT_CONTENT, T_CMT_END,
    T_LIST_START, T_LIST_END,
    T_BLANK, T_NEWLINE,
    T_SYMBOL, T_STRING, T_INT,
    T_FLOAT
};

extern const char *token_type_str[];

#define TOKEN_TYPE_STR(type) token_type_str[type]

#define NEWLINE_CHR    '\n'
#define CMT_START_CHR  ';'
#define CMT_END_CHR     NEWLINE_CHR
#define LIST_START_CHR '('
#define LIST_END_CHR   ')'
#define STRING_START_CHR   '\"'
#define STRING_END_CHR     '\"'
#define STRING_ESCAPE_CHR   '\\'

struct token {
    enum token_type type;
    size_t len;
    bool is_str;
    union {
        char *str;
        struct {
            char chr;
            char null; // if
        };
    };
};
typedef struct token token;

DEF_VECTOR_HEADER(token, struct token);

int tokenize_str(const char *str, vector_token *tokens);
void token_print(const token *tok);
void token_free(token *tok);

#ifdef MOCKA_TEST
#warning COMPILING FOR MOCKA TESTS (HEADER)
ssize_t sstr_len(const char *str);
#endif /* MOCKA_TEST */

#endif /* _TOKEN_H_ */
