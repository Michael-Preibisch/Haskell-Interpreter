void printInteger(int x) {
	print(x);
}

int main () {

  int y;
  printInteger(y, y);	//	Type error. printInteger takes int as parameter
  return 0;
}
