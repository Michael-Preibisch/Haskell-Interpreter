CC = ghc
CFLAGS = -Wall

OBJ =\
    AbsMokka.o \
    Evaluator.o \
	TypeChecker.o \
    LexMokka.o \
    ParMokka.o \
	PrintMokka.o \
	ErrM.o

all: main

clean:
       rm -f *.o main
.c.o:
       $(CC) -c $(INCLUDES) $(CFLAGS) $<

main: $(OBJ)
       $(CC) $(OBJ) -o main
