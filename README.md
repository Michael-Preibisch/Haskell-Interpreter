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

Language supports:

- Three value types -  Int, Bool, String   and a Void type for procedures.
- Arithmetic evaluation.
- While loop construction.
- Pascal-like  `for _ to _ do ` and `for _ downto _ do`  loops construction.
- `If` and `if-else` constructions.
- Built-in printing procedure:  print(int i) / print(boolean b) / print(string s)
- Built-in conversion functions:  toInt, toString.
- Static typing (program is checked for type correctness before evaluation).
- Runtime error handling (such as division by 0).
- Functions (returning values) and procedures,  with recursion.


# Examples


## Code testing multiple functionalities:

```c++

int fact(int i) {
    if (i <= -7 + 2 * 4)        // 'If' statement without else. Rhs of "<=" evaluates to 1
        return 1;
	return i * fact(i - 1);    // Recursion.
}

int main () {
    int i; // No-assign declaration with default value 0.

    print(toInt("123456lololo"));       // 123456 should be printed.
    print(toInt(false));                // 0 should be printed.
    print(toInt(true));                 // 1 should be printed.
    // Factorials of first 10 integers will be printed.
    while (i < 9 + 1) {
        i = i + 1;
        print(fact(i));     // Note the functions composition (int -> int -> IO).
    }

    string x = "Sample string!";     // Declaration with assign.
    print(x);

    if (i != 10) {  // i should have value 10 so this block will not be executed
        print("This should not be executed!");
    }
    else { 		    // if-else statement
        int k;
        print("==== NUMBERS FROM 100 to 90 SHOULD BE PRINTED ====")
        for k = i*i downto ((toInt("10")) * 9) do // built-in function (String -> Int)
            print(toString(k));                   // built-in function (Int -> String)
    }
    printInteger(666);
    return 0;
}

void printInteger(int x) {	// Procedure. Declared AFTER MAIN.
    print(x);               // No return. It's ok because it's a void function.
}

```

## Code testing loops:

```c++
int main() {
    int i = 666;
    int k;

    for k = 10 downto 11 do
        print("This will not be executed!");

    for k = 11 to 10 do
        print("This will not be executed");

    while(2 < 1)
        print("This will not be executed");

    while(i > 0) {
        i = i + 1;
        print("This will be executed... forever!");
    }

	return 0;
}

```

### More examples are in `./bad/` and `./good/` directories


# Author:

### Michał Preibisch
### michal.preibisch@gmail.com
