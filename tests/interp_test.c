#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
#include <cmocka.h>

#include "../token.h"
#include "../interp.h"


static void assert_repr_equal_(const char *input, const char *expected[], size_t len, bool equal)
{
    assert_false(interp_init());

    vector_token tokens;
    assert_false(vector_init_token(&tokens));
    tokenizer_ctx ctx;
    memset(&ctx, 0, sizeof(ctx));
    assert_false(tokenize_str(input, &tokens, &ctx));

    lsp_list *ast = ast_build(&tokens);
    assert_non_null(ast);

    lsp_list *rlst = ast_execute(ast);
    assert_non_null(rlst);

    char *buf = NULL;
    size_t buf_s = 0;
    for (size_t i = 0; i < len; i++) {
        assert_false(lsp_obj_repr_str(rlst->vec.data[i], &buf, &buf_s));
        if (equal) {
            assert_string_equal(buf, expected[i]);
        } else {
            assert_string_not_equal(buf, expected[i]);
        }
        memset(buf, 0, buf_s);
    }
    free(buf);

    lsp_obj_destroy((lsp_obj *) ast);
    //free(ast);
    lsp_obj_pool_release_obj((lsp_obj *) ast);
    lsp_obj_destroy((lsp_obj *) rlst);
    //free(rlst);
    lsp_obj_pool_release_obj((lsp_obj *) rlst);

    while (tokens.len > 0 && !tokens.error) {
        token token = vector_pop_token(&tokens);
        token_destroy(&token);
    }
    assert_false(vector_destroy_token(&tokens));

    assert_false(interp_destroy());
}

static void assert_repr_equal(const char *input, const char *expected[], size_t len)
{
    return assert_repr_equal_(input, expected, len, true);
}

static void assert_repr_not_equal(const char *input, const char *expected[], size_t len)
{
    return assert_repr_equal_(input, expected, len, false);
}

// test interpreting simple usage of the repl function
static void repr_simple(void **state) {
    assert_repr_equal("(repr 1 (2 3 (4 (5))))", (const char *[]) {
            "#str:\"#int:1 #list:(#int:2 #int:3 #list:(#int:4 #list:(#int:5)))\""
                },
        1);
}

// test sum
static void sum_integers_simple(void **state) {
    assert_repr_equal("(+ 1 -3 9)", (const char *[]) {"#int:7"}, 1);
    assert_repr_equal("(+ -1 -3 9)", (const char *[]) {"#int:5"}, 1);
    assert_repr_equal("(+ +0 -1 -3 9)", (const char *[]) {"#int:5"}, 1);
    assert_repr_equal("(+ (+ 1 2 3) (+ 2 3 4))", (const char *[]) {"#int:15"}, 1);
}

// test minus
static void minus_integers_simple(void **state) {
    assert_repr_equal("(- 1 2)", (const char *[]) {"#int:-1"}, 1);
    assert_repr_equal("(- (+ -1 -3) 9)", (const char *[]) {"#int:-13"}, 1);
    assert_repr_equal("(- -1 9)", (const char *[]) {"#int:-10"}, 1);
    assert_repr_equal("(- -1 -9)", (const char *[]) {"#int:8"}, 1);
}

// quote using 'quote' list
static void quote_long_simple(void **state) {
    assert_repr_equal("(quote (+ 1 2 3))", (const char *[]) {"#list:(#sym:+ #int:1 #int:2 #int:3)"}, 1);
    assert_repr_not_equal("(quote (+ 1 2 3))", (const char *[]) {"#list:(#int:6)"}, 1);
    assert_repr_equal("(quote (quote (+ 1 2 3)))", (const char *[]) {"#list:(#sym:quote #list:(#sym:+ #int:1 #int:2 #int:3))"}, 1);
}

// quote using the ' shorthand
static void quote_short_simple(void **state) {
    assert_repr_equal("'(+ 1 2 3)", (const char *[]) {"#list:(#sym:+ #int:1 #int:2 #int:3)"}, 1);
    assert_repr_not_equal("'(+ 1 2 3)", (const char *[]) {"#list:(#int:6)"}, 1);
    assert_repr_equal("'(quote (+ 1 2 3))", (const char *[]) {"#list:(#sym:quote #list:(#sym:+ #int:1 #int:2 #int:3))"}, 1);
    assert_repr_equal("'('(+ 1 2 3))", (const char *[]) {"#list:(#list:(#sym:quote #list:(#sym:+ #int:1 #int:2 #int:3)))"}, 1);
    // TODO fix:
    //assert_repr_equal("(eval '()))", (const char *[]) {"#list:()"}, 1);
    //assert_repr_equal("'()'", (const char *[]) {"#list:()"}, 1);
}

//  cmp
static void cmp_gt(void **state) {
    assert_repr_equal("(< 0 1)", (const char *[]) {"#int:1"}, 1);
    assert_repr_equal("(< 0 1)", (const char *[]) {"#int:1"}, 1);
}

int main(void)
{
    const struct CMUnitTest tests[] = {
        // repr
        cmocka_unit_test(repr_simple),
        cmocka_unit_test(sum_integers_simple),
        cmocka_unit_test(minus_integers_simple),
        cmocka_unit_test(quote_long_simple),
        cmocka_unit_test(quote_short_simple),
        cmocka_unit_test(cmp_gt),
    };
    return cmocka_run_group_tests(tests, NULL, NULL);    
}


