PKG_LIBS = cmocka
LIBS := $(LIBS) `pkg-config --libs $(PKG_LIBS)`
CFLAGS = -g

.PHONY: clean install vector_test token_test runall

vector_test:
	$(CC) $(CFLAGS) -DMOCKA_TEST -o vector_test vector_test.c $(LIBS)

token_test:
	$(CC) $(CFLAGS) -DMOCKA_TEST -o token.o -c ../token.c
	$(CC) $(CFLAGS) -DMOCKA_TEST -o utils.o -c ../utils.c
	$(CC) $(CFLAGS) -DMOCKA_TEST -o token_test token_test.c token.o utils.o $(LIBS)

runall: runvectortest runtokentest

runvectortest: vector_test
	./vector_test

runtokentest: token_test
	./token_test

clean:
	rm vector_test token_test token.o
