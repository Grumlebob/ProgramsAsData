void main() {
  print f(100000);
}
int f(int n) {
  int r;
  r = 0;
  while (n > 0) {
    r = r + (42*2-34*2+32+3);
    n = n-1;
  }
  return r;
}