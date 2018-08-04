#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
#include <cmocka.h>

#include "../token.h"

// sstr_len of string
static void sstr_len_simple(void **state) {
    assert_int_equal(sstr_len("test\""), 4);
    assert_int_equal(sstr_len("___abc___\""), 9);
}

// sstr_len of string with escaped escapes 
static void sstr_len_simple_escaped(void **state) {
    assert_int_equal(sstr_len("\\\\\""), 1);
}

// sstr_len of string with escaped quotes 
static void sstr_len_simple_escaped_quotes(void **state) {
    assert_int_equal(sstr_len("\\\"\""), 1);
    assert_int_equal(sstr_len("\\\\\\\"\""), 2);
}

// sstr_len of string with escaped escapes 
static void sstr_len_escaped(void **state) {
    assert_int_equal(sstr_len("\\\\\""), 1);
}

// sstr_len of string of length 0
static void sstr_len_zero(void **state) {
    assert_int_equal(sstr_len("\""), 0);
}

// sstr_len of string with a single escape char
static void sstr_len_single_escaped(void **state) {
    assert_int_equal(sstr_len("\\\\\""), 1);
}

static void assert_token_equal(const token *t1, const token *t2) {
    assert_int_equal(t1->len, t2->len);
    assert_int_equal(t1->type, t2->type);
    assert_int_equal(t1->is_str, t1->is_str);
    if (t1->is_str) {
        //printf("t1: `%s`, t2: `%s`\n", t1->str, t2->str);
        assert_string_equal(t1->str, t2->str);
    } else {
        assert_int_equal(t1->chr, t2->chr);
    }
}

static void assert_tokenize(const char *str, token expected[], size_t len)
{
    vector_token tokens;
    assert_false(vector_init_token(&tokens));
    assert_false(tokenize_str(str, &tokens));
    assert_int_equal(tokens.len, len);

    for (size_t i = 0; i < len; i++) {
        token tok = vector_get_token(&tokens, i);
        assert_false(tokens.error);
        assert_token_equal(&tok, &expected[i]);
    }
    assert_false(vector_destroy_token(&tokens));
}

// parse a simple string
static void tokenize_string_simple(void **state) {
    assert_tokenize("\"test\"", (token []) {
            (token){.type=T_STRING, .is_str=true, .len=4, .str="test"}
        }, 1);
}

// parse a simple string
static void tokenize_string_blanks(void **state) {
    assert_tokenize("\"test  _\"  ", (token []) {
            (token){.type=T_STRING, .is_str=true, .len=7, .str="test  _"},
            (token){.type=T_BLANK, .is_str=true, .len=2, .str="  "},
        }, 2);
}

// parse multiple strings
static void tokenize_string_quoted(void **state) {
    assert_tokenize("\"\\\"\"", (token []) {
            (token){.type=T_STRING, .is_str=true, .len=1, .str="\""},
        }, 1);
}

int main(void)
{
    const struct CMUnitTest tests[] = {
        // sstr_len
        cmocka_unit_test(sstr_len_zero),
        cmocka_unit_test(sstr_len_simple),
        cmocka_unit_test(sstr_len_simple_escaped),
        cmocka_unit_test(sstr_len_simple_escaped_quotes),
        cmocka_unit_test(sstr_len_escaped),
        cmocka_unit_test(sstr_len_single_escaped),

        // tokenize_str
        cmocka_unit_test(tokenize_string_simple),
        cmocka_unit_test(tokenize_string_blanks),
        cmocka_unit_test(tokenize_string_quoted),
    };
    return cmocka_run_group_tests(tests, NULL, NULL);    
}

