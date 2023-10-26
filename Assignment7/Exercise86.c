void main()
{
 int days;
 int month;
 int y;
 month = 2;
 y = 4;
 switch (month) {
 case 2:
  { days = 28; if (y%4==0) days = 29; }
 case 3:
  { days = 31; }
 case 1:
  { days = 31; }
 }
 print days;
}