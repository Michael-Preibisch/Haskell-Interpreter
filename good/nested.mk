int f(int x, int y) {	// This function used h recursively
	if (x == 0)
		return 0;

	return y + h(x - 1, y);
}

int h(int x, int y) { 	// This function uses f recursively
	if (x == 0)
		return 0;
	return y + f(x - 1, y);
}

int main() {
	print(h(5, 2)); // 10 should be printed - f and h are just multiplication
	return 0;		// where 1 argument is positive.
}
