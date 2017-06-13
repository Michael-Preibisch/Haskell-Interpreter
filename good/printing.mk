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
	print("------")
	print(x);
	print("----- after_else");
	return x + rec(x - 1);
}


int main() {
	print(rec(3));
	return 42;
}
