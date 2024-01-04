// micro-C example 01 for January 2021 exam
// Testing conditional expression as lvalue.
void main() {
  int x[1]; x[0] = 1;
  int y[1]; y[0] = 2;
  (x[0] < y[0] ? x[0] : y[0]) = 3;
  (x[0] < y[0] ? x[0] : y[0]) = 4;
  print x[0]; println; // Expected 3
  print y[0]; println; // Expected 4
}