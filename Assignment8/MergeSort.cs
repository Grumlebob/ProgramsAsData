namespace Assignment8;

public class MergeSort
{
    private static int[] aux;

    static void Main(string[] args)
    {
        
        var arr = Enumerable.Range(0, 10000).Reverse().ToArray();
        Sort(arr, 0, arr.Length - 1); //makes a lot of small temp arrays
        aux = new int[arr.Length];
        SortEfficient(arr, 0, arr.Length - 1); //uses one big aux array
        for (int i = 0; i < arr.Length - 1; i++)
        {
            if (arr[i] > arr[i + 1])
            {
                Console.WriteLine("NOT Sorted!!");
            }
        }
        Console.WriteLine("DONE");
        Console.ReadLine();
    }

    public static void Sort(int[] arr, int start, int end)
    {
        if (start >= end)
        {
            return;
        }
        
        var mid = start + ((end - start) >> 1);
        Sort(arr, start, mid);
        Sort(arr, mid + 1, end);
        Merge(arr, start, mid, mid+1, end);
        
    }

    public static void Merge(int[] arr, int start1, int end1, int start2, int end2)
    {
        int[] temp = new int[end2 - start1 + 1];
        int first = start1;
        int i = 0;
        while (i < temp.Length)
        {
            if (start1 <= end1 && (start2 > end2 || arr[start1] < arr[start2]))
            {
                temp[i] = arr[start1++];
            }
            else
            {
                temp[i] = arr[start2++];
            }
            i++;
        }

        i = 0;
        for (int j =first; i< temp.Length; j++)
        {
            arr[j] = temp[i++];
        }
    }
    
    public static void SortEfficient(int[] arr, int start, int end)
    {
        if (start >= end)
        {
            return;
        }
        
        var mid = start + ((end - start) >> 1);
        SortEfficient(arr, start, mid);
        SortEfficient(arr, mid + 1, end);
        MergeEfficient(arr, start, mid, mid+1, end);
        
    }

    public static void MergeEfficient(int[] arr, int start1, int end1, int start2, int end2)
    {
        int first = start1;
        int i = first;
        while (i <= end2)
        {
            if (start1 <= end1 && (start2 > end2 || arr[start1] < arr[start2]))
            {
                aux[i] = arr[start1++];
            }
            else
            {
                aux[i] = arr[start2++];
            }
            i++;
        }

        for (int j = first; j <= end2; j++)
        {
            arr[j] = aux[j];
        }
    }
    
}