// micro-C example on alias for exam 2021
// Based on example ex3.c
int i;
void main(int n) {
  i=0;
  alias j as i; // Make local variable j an alias of local variable i
  while (j < n) {
    print j;
    i=i+1;
  }
}