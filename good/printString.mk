string f() {
	return "Hello world!";
}

void g() {
	int x;
	x = 10;
	printInt(x);
}

boolean h() {
	return (1 < 2);
}

int main() {
	printString(f());
	g();
	printBool(h());
	return 42;
}
