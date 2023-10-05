/*
(i) Write a micro-C program containing a function void arrsum(int n, int
arr[], int *sump) that computes and returns the sum of the first n elements
of the given array arr. The result must be returned through the sump pointer. The
program’s main function must create an array holding the four numbers 7, 13, 9, 8,
call function arrsum on that array, and print the result using micro-C’s non-standard
print statement.

(ii) Write a micro-C program containing a function void squares(int n,
int arr[]) that, given n and an array arr of length n or more fills arr[i]
with i*i for i = 0, . . . , n − 1.
Your main function should allocate an array holding up to 20 integers, call
function squares to fill the array with n square numbers (where n ≤ 20 is given
as a parameter to the main function), then call function arrsum above to compute
the sum of the n squares, and print the sum.

 */
int* sump;

void main(int maini)
{
    int b[20];
    squares(maini, b);
    arrsum(maini, b, sump);
    print *sump;
}

void squares(int n, int arr[])
{
    int i;
    i = n;
    for(i = n - 1; i >= 0; i = i - 1)
    {
        arr[i] = i * i;
    }
}




void arrsum(int n, int arr[], int* sump)
{
    //sum of first n elements
    int sum;
    sum = 0;
    int i;
    for (i = n - 1; i >= 0; i = i - 1)
    {
        sum = sum + arr[i];
    }
    *sump = sum;
}
