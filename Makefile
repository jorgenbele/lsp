# Author: Jørgen Bele Reinfjell
# File: Makefile
#
# Makefile taken from: https://www.cs.swarthmore.edu/~newhall/unixhelp/howto_makefiles.html
#
# 'make'        build executable file 'mycc'
# 'make clean'  removes all .o and executable files
#

# define the C compiler to use
CC=clang

# define any compile-time flags
CFLAGS := $(CFLAGS) -g -Og -Wall -Werror
LIBS :=
SRCS = lsp.c token.c builtins.c interp.c types.c utils.c
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
	$(RM) modules/*.o *~
