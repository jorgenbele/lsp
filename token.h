#ifndef _TOKEN_H_
#define _TOKEN_H_

#include <stdbool.h>
#include <ctype.h>

#include "vector.h"

/**
 * comment:    ';' .* NEWLINE
 * symbol:     [a-zA-Z_+*-/#%][0-9a-zA-Z_+*-/#%]
 * string:     '"' ([\.] | .)* '"'
 * int:        [+-]?[1-9][0-9]*
 * float:      int '.' [0-9][0-9]*
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
    T_FLOAT,
    T_QUOTE
};

extern const char *token_type_str[];

#define TOKEN_TYPE_STR(type) token_type_str[type]

#define NEWLINE_CHR       '\n'
#define CMT_START_CHR     ';'
#define CMT_END_CHR       NEWLINE_CHR
#define LIST_START_CHR    '('
#define LIST_END_CHR      ')'
#define STRING_START_CHR  '\"'
#define STRING_END_CHR    '\"'
#define STRING_ESCAPE_CHR '\\'

#define IS_SYMBOL_START_CHR(c)                  \
    (isalpha((c)) || (c) == '_'                 \
     || (c) == '+' || (c) == '-'                \
     || (c) == '/' || (c) == '*'                \
     || (c) == '#' || (c) == '%')

#define IS_SYMBOL_CHR(c) (IS_SYMBOL_START_CHR((c)) || isdigit((c))) 

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

int tokenize_str__(const char *str, vector_token *tokens, const char **last);
// DEPRECATED
int tokenize_str(const char *str, vector_token *tokens);
void token_print(const token *tok);
void token_destroy(token *tok);

#ifdef MOCKA_TEST
ssize_t sstr_len(const char *str);
#endif /* MOCKA_TEST */

#endif /* _TOKEN_H_ */
