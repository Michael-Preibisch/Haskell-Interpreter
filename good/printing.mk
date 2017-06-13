string f() {
	return "Hello world!";
}

int g(int x, int y) {
	return x - y;
}


boolean h() {
	if (1 < 2) {
		return true;
	}
	else {
		print("lel");
		return false;
	}
}

boolean x() {
	return !h();
}

int rec(int x) {
	if (x < 1) {
		return 0;
	}
	print(x);
	print("-----");
	x = x - 1;
	return x + 1 + rec(x);
}


int main() {
	int x = 10;

	if (x > 1)
		print(x);

//	print(rec(x));
//	print(x);
//	return 42;
}
