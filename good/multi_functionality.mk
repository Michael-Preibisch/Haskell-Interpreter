void printInteger(int x) {	// Procedure.
    print(x);   // No return
}

int fact(int i) {
    if (i <= -7 + 2 * 4) // 'If' statement without else. Rhs of "<=" evaluates to 1
        return 1;
	return i * fact(i - 1); // Recursion.
}

int main () {
    int i; // No-assign declaration with default value 0.

    print(toInt("123456lololo")); // 123456 should be printed.
    print(toInt(false));    // 0 should be printed.
    print(toInt(true));     // 1 should be printed.
    // Factorials of first 10 integers will be printed.
    while (i < 9 + 1) {
        i = i + 1;
        print(fact(i));
        // Note the functions composition (int -> int -> IO).
    }

    string x = "Sample string!";     // Declaration with assign.
    print(x);

    if (i != 10) {  // i should have value 10 so this block will not be executed
        print("This should not be executed!");
    }
    else { 			// if-else statement
        int k;
        print("==== NUMBERS FROM 100 to 90 SHOULD BE PRINTED ====")
        for k = i*i downto ((toInt("10")) * 9) do // built-in function (String -> Int)
            print(toString(k));                   // built-in function (Int -> String)
    }

    return 0;
}
