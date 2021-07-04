
int fib(int i) {
  if(i <= 2) {
    return 1;
  } else {
    return fib(i - 2) + fib(i - 1);
  }
}

int main(void) {
  return fib(6);
}
