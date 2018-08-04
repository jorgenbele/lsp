#include <stdio.h>

#include "../vector.h"

#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
#include <cmocka.h>

DEF_VECTOR(int, int, 0xBEEF);
typedef char * char_ptr;
DEF_VECTOR(char_ptr, char_ptr, NULL);

// initiating vector from array
static void test_vector_init_w_int(void **state) {
    vector_int ints;
    assert_false(vector_init_w_int(&ints, (const int []){1, 2, 3}, 3));
    assert_int_equal(ints.len, 3);
    assert_int_equal(ints.size, 3);
    assert_int_equal(vector_get_int(&ints, 0), 1);
    assert_int_equal(vector_get_int(&ints, 1), 2);
    assert_int_equal(vector_peek_int(&ints), 3);
}

// pushing to vector
static void test_vector_push_int(void **state) {
    vector_int ints;
    assert_false(vector_init_int(&ints));

    for (size_t i = 0; i < 10e3; i++) {
        assert_false(vector_push_int(&ints, i));
        assert_int_equal(ints.len, i+1);
        assert_true(ints.size >= i);
    }

    vector_destroy_int(&ints);
}

// popping from vector
static void test_vector_pop_int(void **state) {
    vector_int ints;
    assert_false(vector_init_int(&ints));

    for (size_t i = 0; i < 10e3; i++) {
        assert_false(vector_push_int(&ints, i));
        assert_int_equal(ints.len, i+1);
        assert_true(ints.size >= i);
    }

    while (ints.len > 0) {
        assert_int_equal(vector_pop_int(&ints), ints.len-1);
    }

    assert_int_equal(ints.len, 0);
    assert_int_equal(vector_pop_int(&ints), 0xBEEF);
    assert_int_equal(vector_pop_int(&ints), 0xBEEF);

    assert_false(vector_destroy_int(&ints));
}

// getting from vector
static void test_vector_get_int(void **state) {
    vector_int ints;
    assert_false(vector_init_int(&ints));

    for (size_t i = 0; i < 10e3; i++) {
        assert_false(vector_push_int(&ints, i));
        assert_int_equal(ints.len, i+1);
        assert_true(ints.size >= i);
    }

    for (size_t i = 0; i < 10e3; i++) {
        assert_int_equal(vector_get_int(&ints, i), i);
    }

    assert_false(vector_destroy_int(&ints));
}

// peeking from vector
static void test_vector_peek_int(void **state) {
    vector_int ints;
    assert_false(vector_init_int(&ints));

    for (size_t i = 0; i < 10e3; i++) {
        assert_false(vector_push_int(&ints, i));
        assert_int_equal(ints.len, i+1);
        assert_true(ints.size >= i);
        assert_int_equal(vector_peek_int(&ints), i);
    }

    assert_false(vector_destroy_int(&ints));
}

// initiating vector from array
static void test_vector_init_w_char_ptr(void **state) {
    vector_char_ptr char_ptrs;
    const char_ptr strings[] = {"This", "is", "a", "test"};
    assert_false(vector_init_w_char_ptr(&char_ptrs, strings, 4));
    assert_int_equal(char_ptrs.len, 4);
    assert_int_equal(char_ptrs.size, 4);
    assert_string_equal(vector_get_char_ptr(&char_ptrs, 0), strings[0]);
    assert_string_equal(vector_get_char_ptr(&char_ptrs, 1), strings[1]);
    assert_string_equal(vector_get_char_ptr(&char_ptrs, 2), strings[2]);
    assert_string_equal(vector_peek_char_ptr(&char_ptrs), strings[3]);
}

// pushing to vector
static void test_vector_push_char_ptr(void **state) {
    vector_char_ptr char_ptrs;
    assert_false(vector_init_char_ptr(&char_ptrs));

    for (size_t i = 0; i < 10e3; i++) {
        assert_false(vector_push_char_ptr(&char_ptrs, "test"));
        assert_int_equal(char_ptrs.len, i+1);
        assert_true(char_ptrs.size >= i);
    }

    vector_destroy_char_ptr(&char_ptrs);
}

// popping from vector
static void test_vector_pop_char_ptr(void **state) {
    vector_char_ptr char_ptrs;
    assert_false(vector_init_char_ptr(&char_ptrs));

    for (size_t i = 0; i < 10e3; i++) {
        assert_false(vector_push_char_ptr(&char_ptrs, "test"));
        assert_int_equal(char_ptrs.len, i+1);
        assert_true(char_ptrs.size >= i);
    }

    while (char_ptrs.len > 0) {
        assert_string_equal(vector_pop_char_ptr(&char_ptrs), "test");
    }

    assert_int_equal(char_ptrs.len, 0);
    assert_null(vector_pop_char_ptr(&char_ptrs));
    assert_null(vector_pop_char_ptr(&char_ptrs));
    assert_false(vector_destroy_char_ptr(&char_ptrs));
}

// getting from vector
static void test_vector_get_char_ptr(void **state) {
    vector_char_ptr char_ptrs;
    assert_false(vector_init_char_ptr(&char_ptrs));

    for (size_t i = 0; i < 10e3; i++) {
        assert_false(vector_push_char_ptr(&char_ptrs, "test"));
        assert_int_equal(char_ptrs.len, i+1);
        assert_true(char_ptrs.size >= i);
    }


    for (size_t i = 0; i < 10e3; i++) {
        assert_string_equal(vector_get_char_ptr(&char_ptrs, i), "test");
    }

    assert_false(vector_destroy_char_ptr(&char_ptrs));
}

// peeking from vector
static void test_vector_peek_char_ptr(void **state) {
    vector_char_ptr char_ptrs;
    assert_false(vector_init_char_ptr(&char_ptrs));

    for (size_t i = 0; i < 10e3; i++) {
        assert_false(vector_push_char_ptr(&char_ptrs, "test"));
        assert_int_equal(char_ptrs.len, i+1);
        assert_true(char_ptrs.size >= i);
        assert_string_equal(vector_peek_char_ptr(&char_ptrs), "test");
    }

    vector_destroy_char_ptr(&char_ptrs);
}


int main(void) {
    const struct CMUnitTest tests[] = {
        // ints
        cmocka_unit_test(test_vector_push_int),
        cmocka_unit_test(test_vector_pop_int),
        cmocka_unit_test(test_vector_get_int),
        cmocka_unit_test(test_vector_peek_int),
        cmocka_unit_test(test_vector_init_w_int),

        // char pointers
        cmocka_unit_test(test_vector_push_char_ptr),
        cmocka_unit_test(test_vector_pop_char_ptr),
        cmocka_unit_test(test_vector_get_char_ptr),
        cmocka_unit_test(test_vector_peek_char_ptr),
        cmocka_unit_test(test_vector_init_w_char_ptr),
    };
    return cmocka_run_group_tests(tests, NULL, NULL);    
}

#if 0
DEF_VECTOR(int, int, -1);
typedef char * char_ptr;

DEF_VECTOR(char_ptr, char_ptr, NULL);

int main(int argc, const char *argv[]) {
    printf("HELLO WORLD!\n");

    vector_int vec;
    if (vector_init_w_int(&vec, (const int []) {1, 2, 3}, 3)) {
        printf("ERROR!");
    }
    printf("Size: %lu, len: %lu, allocs:%lu\n", vec.size, vec.len, vec.allocs);

    for (size_t i = 0; i < vec.len; i++) {
        printf("%lu: %d\n", i, vector_get_int(&vec, i));
    }

    for (size_t i = 0; i < 10e4; i++) {
        vector_push_int(&vec, i);
    }
    printf("DONE!\n");
    printf("Size: %lu, len: %lu, allocs:%lu\n", vec.size, vec.len, vec.allocs);

    vector_destroy_int(&vec);

    vector_char_ptr vec_str;
    if (vector_init_w_char_ptr(&vec_str, (const char_ptr []){"Hello", "little", "one"}, 3)) {
        printf("ERROR!");
    }

    vector_push_char_ptr(&vec_str, "test");

    for (size_t i = 0; i < vec_str.len; i++) {
        printf("%lu: %s\n", i, vector_get_char_ptr(&vec_str, i));
    }

    vector_destroy_char_ptr(&vec_str);
}
#endif
