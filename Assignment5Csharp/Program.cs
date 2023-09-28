/*
 * (B) Implement a similar Java (or C#) method
static int[] merge(int[] xs, int[] ys)
that takes two sorted arrays of ints and merges them into a sorted array of ints. The
method should build a new array, and should not modify the given arrays. Two arrays
xs and ys of integers may be built like this:
int[] xs = { 3, 5, 12 };
int[] ys = { 2, 3, 4, 7 };

*/

int[] xs = { 3, 5, 12 };
int[] ys = { 2, 3, 4, 7 };

int[] Merge(int[] xs, int[] ys)
{
    int[] zs = new int[xs.Length + ys.Length];
    int xsPointer = 0, ysPointer = 0;
    for (int i = 0; i < zs.Length; i++)
    {
        //If there are more elements in both xs and ys, compare them and add the smallest to result.
        if (xsPointer < xs.Length && ysPointer < ys.Length)
        {
            if (xs[xsPointer] < ys[ysPointer])
            {
                zs[i] = xs[xsPointer];
                xsPointer++;
            }
            else
            {
                zs[i] = ys[ysPointer];
                ysPointer++;
            }
        }
        //Just add all ys to rest of result, as xs is empty.
        else if (xsPointer == xs.Length)
        {
            zs[i] = ys[ysPointer];
            ysPointer++;
        }
        //Just add all xs to rest of result, as ys is empty.
        else if (ysPointer == ys.Length)
        {
            zs[i] = xs[xsPointer];
            xsPointer++;
        }
    }

    return zs;
}

int[] mergeResult = Merge(xs, ys);
foreach (int z in mergeResult)
{
    Console.WriteLine(mergeResult);
}