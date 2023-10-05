/*
(iii) Write a micro-C program containing a function void histogram(int n,
int ns[], int max, int freq[]) which fills array freq the frequencies
of the numbers in array ns. More precisely, when the function returns, element
freq[c] must equal the number of times that value c appears among the first n
elements of arr, for 0<=c<=max. You can assume that all numbers in ns are
between 0 and max, inclusive.
For example, if your main function creates an array arr holding the seven numbers
1 2 1 1 1 2 0 and calls histogram(7, arr, 3, freq), then afterwards
freq[0] is 1, freq[1] is 4, freq[2] is 2, and freq[3] is 0. Of course,
freq must be an array with at least four elements. What happens if it is not?
---Our answer: In the function it must reset the freq array for each index. So all indices between 0 and max.

The array freq should be declared and allocated in the main function, and passed to
histogram function. It does not work correctly (in micro-C or C) to stack-allocate
the array in histogram and somehow return it to the main function. Your main
function should print the contents of array freq after the call.
 */
int* sump;

void main(int maini)
{
    int freq[4];
    int arr[7];
    arr[0] = 1;
    arr[1] = 2;
    arr[2] = 1;
    arr[3] = 1;
    arr[4] = 1;
    arr[5] = 2;
    arr[6] = 0;
    histogram(7, arr, 3, freq);
    int i;
    i = 0;
    while (i <= 3)
    {
        print freq[i];
        i = i + 1;
    }
}

void histogram(int n, int ns[], int max, int freq[])
{
    int j;
    j = 0;
    while(j <= max)
    {
        freq[j] = 0;
        j = j + 1;
    }
    int i;
    i = 0;
    while (i < n)
    {
        int k;
        k = ns[i];
        freq[k] = freq[k] + 1;
        i = i + 1;
    }
}