PKG_LIBS = cmocka
LIBS := $(LIBS) `pkg-config --libs $(PKG_LIBS)`
CFLAGS = -g
#CC = musl-clang
#CFLAGS := $(CFLAGS) -O3 -Wall -Wpedantic -std=c1
#7 -fPIC
CFLAGS := $(CFLAGS) -O3 -Wall -Wextra -Wno-missing-braces -Wno-unused-parameter -Wpedantic -std=c17

.PHONY: all clean install vector_test token_test runall

all: vector_test token_test interp_test

vector_test:
	$(CC) $(CFLAGS) -DMOCKA_TEST -o vector_test vector_test.c $(LIBS)

token_test:
	$(CC) $(CFLAGS) -DMOCKA_TEST -o token.o -c ../token.c
	$(CC) $(CFLAGS) -DMOCKA_TEST -o utils.o -c ../utils.c
	$(CC) $(CFLAGS) -DMOCKA_TEST -o token_test token_test.c token.o utils.o $(LIBS)

interp_test:
	$(CC) $(CFLAGS) -DMOCKA_TEST -o token.o -c ../token.c
	$(CC) $(CFLAGS) -DMOCKA_TEST -o utils.o -c ../utils.c
	$(CC) $(CFLAGS) -DMOCKA_TEST -o repl.o -c ../repl.c
	$(CC) $(CFLAGS) -DMOCKA_TEST -o builtins.o -c ../builtins.c
	$(CC) $(CFLAGS) -DMOCKA_TEST -o types.o -c ../types.c
	$(CC) $(CFLAGS) -DMOCKA_TEST -o interp.o -c ../interp.c
	$(CC) $(CFLAGS) -DMOCKA_TEST -o interp_test interp_test.c token.o utils.o repl.o builtins.o types.o interp.o $(LIBS)

runall: runvectortest runtokentest runinterptest

runvectortest: vector_test
	./vector_test

runtokentest: token_test
	./token_test

runinterptest: interp_test
	./interp_test

clean:
	rm vector_test token_test interp_test *.o 
