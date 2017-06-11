void printInteger(int x) {
	printInt(x);
	return;
}

int fact(int i) {
	if (i <= -7 + 2 * 4) // "If statement without else. Rhs of "<=" evaluates to 1
		return 1;
	return i * fact(i - 1); // Recursion
}

int main () {
  int i;
  // Factorials of first 10 integers will be printed.
  for i = 1 to (9 + 1) do {
      printInt(fact(i)); // Note the functions composition (int -> int -> IO)
  }

  if (i != 10)  // i should have value 10 so this block will not be executed
    printString("This should not be executed!");
  else { 			// if-else statement
    int k;
	for k = i*i downto stringToInt("10") do 	// built-in function (String -> Int)
      printString(intToString(k));			// built-in function (Int -> String)
  }

  return 0;
}