// micro-C example 01 for January 2021 exam
// Testing conditional expression as lvalue.
void main() {
  int a;
  int b;
  int *x;
  int *y;
  x = &a;
  y = &b;
  (*x) = 1;
  (*y) = 2;
  ((*x) < (*y) ? (*x) : (*y)) = 3;
  ((*x) < (*y) ? (*x) : (*y)) = 4;
  print (*x); println; // Expected 3
  print (*y); println; // Expected 4
}