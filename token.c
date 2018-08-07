#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <ctype.h>
#include <assert.h>

#include "token.h"
#include "utils.h"


#ifdef MOCKA_TEST
#define STATIC
#else
#define STATIC static
#endif

const char *token_type_str[] = {
    "T_UNKNOWN",
    "T_CMT_START", "T_CMT_CONTENT", "T_CMT_END",
    "T_LIST_START", "T_LIST_END",
    "T_BLANK", "T_NEWLINE",
    "T_SYMBOL", "T_STRING", "T_INT",
    "T_FLOAT",
    "T_QUOTE",
};

//static const struct token default_token = {T_UNKNOWN, 0};
DEF_VECTOR_FUNCS(token, struct token, ((const struct token) {T_UNKNOWN, 0}));


#if 0
static bool expect(const char *str, const char *expected)
{
    if (!strstr(str, expected)) {
        return false;
    }
    return true;
}
#endif

static const char *parse_comment(const char *str, vector_token *tokens)
{
    const char *ptr = str;

    if (*ptr != CMT_START_CHR) {
        parse_error("Expected '%c' for start of comment, got: '%c'!\n", CMT_START_CHR, *ptr);
        return NULL;
    }

    vector_push_token(tokens, (token)
                        {T_CMT_START, 1, .is_str=false, .chr=CMT_START_CHR});
    // Skip the CMT_START_CHR
    str++;
    ptr++;

    size_t cmnt_len = 0;
    while (*ptr && *ptr != CMT_END_CHR) {
        ptr++;
        cmnt_len++;
    }

    char *cmnt_str = xstrdupn(str, cmnt_len);
    token cmnt = {T_CMT_CONTENT, cmnt_len, .is_str=true, .str=cmnt_str};
    vector_push_token(tokens, cmnt);

    if (*ptr != CMT_END_CHR) {
        parse_error("Expected '%c' for end of comment, got: '%c'!\n", CMT_END_CHR, *ptr);
        return NULL;
    }

    ptr++;
    token cmnt_end = {T_CMT_END, 1, .is_str=false, .chr=CMT_END_CHR, .null=0};
    vector_push_token(tokens, cmnt_end);

    return ptr;
}

static const char *parse_blank(const char *str, vector_token *tokens)
{
    const char *ptr = str;
    size_t blanks_len = 0;
    while (*ptr && isblank(*ptr)) {
        ptr++;
        blanks_len++;
    }

    char *blanks_str = xstrdupn(str, blanks_len);
    token blanks = {T_BLANK, blanks_len, .is_str=true, .str=blanks_str};
    vector_push_token(tokens, blanks);

    return ptr;
}

DEF_VECTOR(char, char, 0);

// takes a string which starts WITHOUT a \" (quote)
STATIC ssize_t sstr_len(const char *str)
{
    const char *ptr = str;
    // Find the length needed to store the string
    size_t len = 0;
    bool escaped = false;
    while (*ptr) {
        if (*ptr == STRING_ESCAPE_CHR) {
            if (!escaped) {
                len++;
                escaped = true;
            } else {
                escaped = false;
            }
        } else if (*ptr == STRING_END_CHR && !escaped) {
            return len;
        } else {
            if (!escaped) {
                // The string length is counted
                // when the first \ is encountered.
                // And we only count it one time.
                len++;
            }
            escaped = false;
        }
        ptr++;
    }
    return -1;
}

static const char *parse_string(const char *str, vector_token *tokens)
{
    if (*str != STRING_START_CHR) {
        return NULL;
    }
    // Skip the STRING_START_CHR
    str++;
    if (!(*str)) {
        // Was not a valid string. Was
        // just a starting quote but
        // nothing else.
        parse_error("Attempting to parse unterminated and empty string: `%s``\n", str);
        return NULL;
    }

    ssize_t slen = sstr_len(str);
    if (slen < 0) {
        parse_error("Attempting to parse unterminated string: `%s``\n", str);
        return NULL;
    }
    size_t len = (size_t) slen;
    char *s = xmalloc(len + 1);
    char *sptr = s;

    const char *ptr = str;
    bool escaped = 0;
    while (*ptr) {
        if (*ptr == STRING_ESCAPE_CHR) {
            if (escaped) {
                *sptr++ = *ptr;
            }
            escaped = !escaped;
        } else if (*ptr == STRING_END_CHR && !escaped) {
            ptr++;
            break;
        } else {
            *sptr++ = *ptr;
            escaped = false;
        }
        ptr++;
    }

    *sptr++ = '\0';
    vector_push_token(tokens, (token)
                      {T_STRING, .len=len, .is_str=true, .str=s});

    return ptr;
}

// Either int or float
static const char *parse_number(const char *str, vector_token *tokens)
{
    const char *ptr = str;
    if (!isdigit(*ptr) && (*ptr != '+' && *ptr != '-')) {
        //parse_error("Illegal start of number: `%s`\n", str);
        return NULL;
    }

    size_t sign_len = 0;
    if (*ptr == '-' || *ptr == '+') {
        sign_len++;
        ptr++;
    }

    size_t int_len = 0;
    while (isdigit(*ptr)) {
        ptr++;
        int_len++;
    }

    if (int_len < 1) {
        // not a valid int
        return NULL;
    }

    bool is_float = false;
    size_t float_len = 0;
    if (*ptr == '.') {
        is_float = true;
        ptr++;
        float_len++;
        while (isdigit(*ptr)) {
            float_len++;
            ptr++;
        }
    }

    size_t tot_len = sign_len + int_len + float_len;
    char *s = xstrdupn(str, tot_len);
    enum token_type ttype = is_float ? T_FLOAT : T_INT;
    token t = {.type=ttype, .is_str=true, .len=tot_len, .str=s};
    assert(!vector_push_token(tokens, t));
    return ptr;
}

static const char *parse_symbol(const char *str, vector_token *tokens)
{
    const char *ptr = str;

    if (!IS_SYMBOL_START_CHR(*ptr)) {
        parse_error("Illegal start char for symbol: `%s`\n", str);
        return NULL;
    }

    size_t len = 0;
    while (*ptr && IS_SYMBOL_CHR(*ptr)) {
        ptr++;
        len++;
    }

    char *s = xstrdupn(str, len);
    token t = {.type=T_SYMBOL, .is_str=true, .len=len, .str=s};
    assert(!vector_push_token(tokens, t));
    return ptr;
}

static const char *parse_atom(const char *str, vector_token *tokens)
{
    const char *ptr = str;

    // Can either be a symbol, string, or a number
    if (*str == STRING_START_CHR) {
        // Must be a string.
        return parse_string(ptr, tokens);
    }

    if (isdigit(*str)) {
        // Must be a number (int or float)
        return parse_number(ptr, tokens);
    }

    // Try parse number.
    const char *p = parse_number(ptr, tokens);
    if (p) {
        return p;
    }

    // Must be a symbol
    return parse_symbol(ptr, tokens);
}

// tokenizes from 'strr until end of list or first symbol.
// updates 'last' to be the pointer to next char
// which has not been read yet.
int tokenize_str_r(const char *str, vector_token *tokens, const char **last)
{
    const char *ptr = str;
    int ret = 0;
    while (*ptr) {
        // List
        if (*ptr == LIST_START_CHR) {
            vector_push_token(tokens, (token)
                              {T_LIST_START, 1, .is_str=false, .chr=LIST_START_CHR});
            ptr++;
            //fprintf(stderr, "LIST_START_CHR rest:`%s`\n", ptr);
            ret = tokenize_str_r(ptr, tokens, last);
            break;
        } else if (*ptr == LIST_END_CHR) {
            vector_push_token(tokens, (token)
                              {T_LIST_END, 1, .is_str=false, .chr=LIST_END_CHR});
            ptr++;
            //fprintf(stderr, "LIST_END_CHR rest:`%s`\n", ptr);
            *last = ptr;
            break;
        // ';' Comments ignore the rest of the line.
        } else if (*ptr == CMT_START_CHR) {
            ptr = parse_comment(ptr, tokens);
            //fprintf(stderr, "CMD_START_CHR rest:`%s`\n", ptr);
            *last = ptr;
            break;
        } else if (*ptr == NEWLINE_CHR) {
            vector_push_token(tokens, (token)
                              {T_NEWLINE, 1, .is_str=false, .chr=NEWLINE_CHR});
            ptr++;
            //fprintf(stderr, "NEWLINE_CHR rest:`%s`\n", ptr);
            *last = ptr;
            break;
        // Blanks
        } else if (isblank(*ptr)) {
            ptr = parse_blank(ptr, tokens);
            //fprintf(stderr, "BLANK_CHR rest:`%s`\n", ptr);
            *last = ptr;
        } else {
            ptr = parse_atom(ptr, tokens);
            if (!ptr)  {
                parse_error("Failed to parse atom.\n");
                break;
            }
            //fprintf(stderr, "ATOM rest:`%s`\n", ptr);
            *last = ptr;
        }
    }

    if (!ptr && !ret) {
        ret = 1;
    }

    return ret;
}

int tokenize_str(const char *str, vector_token *tokens)
{
    size_t len = strlen(str);
    size_t last_i = 0;
    size_t tokens_start = 0;
    while (true) {
        if (last_i >= len) {
            break;
        }
        const char *start = str + last_i;
        const char *last = start;
        if (tokenize_str_r(start, tokens, &last)) {
            break;
        }
        last_i = last - str;
        tokens_start = tokens->len;
    }
    return 0;
}

void token_destroy(token *tok)
{
    if (tok->is_str) {
        free(tok->str);
    }
}

void token_print(const token *tok)
{
    char str[2] = {0, 0};
    char *str_ptr = str;
    switch (tok->type) {
        case T_LIST_START: case T_LIST_END:
        case T_CMT_START: case T_CMT_END:
        case T_UNKNOWN:
        case T_NEWLINE:
        case T_QUOTE:
            str[0] = tok->chr;
            break;

        case T_SYMBOL:
        case T_STRING:
        case T_INT: case T_FLOAT:
        case T_BLANK:
        case T_CMT_CONTENT:
            str_ptr = tok->str;
            break;
    }
    printf("type: %-14s, len: %lu, str: `%s`\n", TOKEN_TYPE_STR(tok->type), tok->len, str_ptr);
}
