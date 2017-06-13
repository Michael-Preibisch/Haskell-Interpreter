int main() {

	int k; // default value 0

	print("");
	print("==== NUMBERS 0 to 5 SHOULD BE PRINTED ====");
	for k = 0 to 5 do
		print(k);

	print("==== NUMBERS 5 to 10 SHOULD BE PRINTED ====");
	for k to 10 do
		print(k);

		print("==== NUMBERS 10 to 5 SHOULD BE PRINTED ====");
	for k downto 5 do
		print(k);

	print("==== NUMBERS 0 to -5 SHOULD BE PRINTED ====");
	for k = 0 downto -5 do
		print(k);

	print("==== NOTHING SHOULD BE PRINTED ====");
	for k downto 10 do
		print(k);

	print("==== NOTHING SHOULD BE PRINTED ====");
	for k to -10 do
		print(k);

	print("==== NUMBERS FROM -5 to 5 SHOULD BE PRINTED ====");
	while (k <= 5) {
		print(k);
		k = k + 1;
	}

	print("==== '6' SHOULD BE PRINTED ====");
	print(k);


	return 0;
}
