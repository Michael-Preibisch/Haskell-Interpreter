# Programming Languages and Paradigms course @ MIMUW

Interpreter for a simple C-like imperative language named "Mokka", written in Haskell.

# Build

To build the project, simply type `make` in project's main directory.
to run, type
```
./interpreter <path_to_program_file>
```

# The language - Mokka

Mokka is simple, imperative, statically typed language with a syntax similar to C.
It is inspired by language "Latte" presented in 'Compiler Construction' course @ MIMUW.

It's features contains:

- Three value types -  Int, Bool, String   and a Void type for procedures.
- Arithmetic evaluation.
- While loop construction.
- Pascal-like  `for _ to _ do ` and `for _ downto _ do`  loops construction.
- `If` and `if-else` constructions.
- Built-in printing procedures:  printInt, printString, printBool.
- Built-in conversion functions:  stringToInt, intToString.
- Static typing (program is checked for type correctness before evaluation).
- Runtime error handling (such as division by 0).
- Functions (returning values) and procedures,  with recursion.
