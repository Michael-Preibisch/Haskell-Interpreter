void printInteger(int x) {	// Procedure.
    print(x);
    return;
}

int fact(int i) {
    print(i);
    if (i <= -7 + 2 * 4) // 'If' statement without else. Rhs of "<=" evaluates to 1
        return 1;
	return i * fact(i - 1); // Recursion.
}

int main () {
    int i = 0; // No-assign declaration with default value 0.

    print(toInt("123456lololo"));
    print(toInt(false));
    print(toInt(true));
    // Factorials of first 10 integers will be printed.
    while (i < 9 + 1) {
        print(i);
        print(fact(i));
        // Note the functions composition (int -> int -> IO).
        i = i + 1;
    }

    string x = "Sample string!";     // Declaration with assign.
    print(x);

    if (i != 10) {  // i should have value 10 so this block will not be executed
        print("This should not be executed!");
    }
    else { 			// if-else statement
        int k;
        for k = i*i downto toInt("10") do     // built-in function (String -> Int)
            print(toString(k));               // built-in function (Int -> String)
    }

    return 0;
}
