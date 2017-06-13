int main () {
  int x = 36288000;
  int i = 10;


  while (i >= -2) {
    i = i - 1;
  	x = x / i;   // Division by 0
  }
  print("THIS SHOULD NOT BE EXECUTED");

  return 0;
}
