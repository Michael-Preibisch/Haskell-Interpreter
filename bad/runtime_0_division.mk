int main () {
  int x = 36288000;
  int i = 10;

  while (i >= 0) {
  	x = x / i;
	i = i - 1;		// Division by 0
  }

  return 0;
}
