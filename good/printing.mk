string f() {
	return "Hello world!";
}

void g() {
	int x;
	x = 10;
	print(x);
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

int main() {
	print(f());
	g();
	print(h());
	return 42;
}
