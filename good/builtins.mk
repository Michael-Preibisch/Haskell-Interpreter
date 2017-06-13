int main() {
	test_builtins(10, false, "abc"); // "False", 0, 0, "True" should be printed

	print(toInt("002342abxc")); // 2342 should be printed.
	print(toInt("_1112")); 		// 0 should be printed.
	return 0;
}

void test_builtins(int x, boolean y, string z) {
	int n;
	string t;
	boolean b;

	t = toString(y);
	n = toInt(z);
	print(t);
	print(n);
	print(toInt(y));
	print(toString(!y));

}
