void main(int maini)
{
 int e;
 e = 10;
 --e;
 print e;
 ++e;
 print e;

 int a[4];
 a[0] = 7;
 a[1] = 13;
 a[2] = 9;
 a[3] = 8;
 int i;
 i = 0;
 while (i < 4)
 {
  print ++a[++i];
 }
}