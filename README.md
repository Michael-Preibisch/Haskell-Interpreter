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


# Example program

```c++

void printInteger(int x) {	// procedure
    printInt(x);
    return;
}

int fact(int i) {
    if (i <= -7 + 2 * 4) // "If statement without else. Rhs of "<=" evaluates to 1
        return 1;
	return i * fact(i - 1); // Recursion
}

int main () {
    int i; // no-assign declaration with defaul value 0

    // Factorials of first 10 integers will be printed.
    for i = 1 to (9 + 1) do {
        printInteger(fact(i)); // Note the functions composition (int -> int -> IO)
    }

    string x = "Sample string!"
    printString(x);

    if (i != 10)  // i should have value 10 so this block will not be executed
        printString("This should not be executed!");
    else { 			// if-else statement
    int k;
    for k = i*i downto stringToInt("10") do 	// built-in function (String -> Int)
        printString(intToString(k));			// built-in function (Int -> String)
    }

    return 0;
}

```
