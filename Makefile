# Author: Jørgen Bele Reinfjell
# File: Makefile
#
# Makefile taken from: https://www.cs.swarthmore.edu/~newhall/unixhelp/howto_makefiles.html
#
# 'make'        build executable file 'mycc'
# 'make clean'  removes all .o and executable files
#

# define the C compiler to use
CC = gcc
#CC=clang
#CC = musl-clang
#CC = musl-gcc
#CC = clang

# define any compile-time flags
#CFLAGS := $(CFLAGS) -g -Og -Wall -Wpedantic #-std=c17 -fPIC
#CFLAGS := $(CFLAGS) -g -Wall -Wpedantic #-std=c17 -fPIC
CFLAGS := $(CFLAGS) -O3 -Wall -Wextra -Wno-missing-braces -Wno-unused-parameter -std=c17 -g
#CFLAGS := $(CFLAGS) -g -Og -Wall -Wpedantic -std=c17 -fPIC -Qunused-arguments
#CFLAGS := $(CFLAGS) -g -Og -Wall -Wpedantic -std=c17 -fPIC -Qunused-arguments
#CFLAGS := $(CFLAGS) -g -Wall -Wpedantic -std=c17 -fPIC # -Qunused-arguments
#CFLAGS := $(CFLAGS) -O3 -Wall -Wpedantic -std=c17 -fPIC #-DUSE_OBJ_POOL
#CFLAGS := $(CFLAGS) -g -Og -Wall -Wpedantic
LIBS :=
SRCS = lsp.c token.c builtins.c interp.c types.c utils.c repl.c
OBJS = $(SRCS:.c=.o)
EXEC = lsp

.PHONY: clean install

all:    $(EXEC)
	@echo  Successfully compiled

$(EXEC): $(OBJS) 
	$(CC) $(CFLAGS) $(INCLUDES) -o $(EXEC) $(OBJS) $(LFLAGS) $(LIBS)

.c.o:
	$(CC) $(CFLAGS) $(INCLUDES) -c $<  -o $@

install: $(EXEC)
	cp $(EXEC) $(HOME)/bin/$(EXEC)

clean:
	$(RM) *.o *~ $(EXEC)
