namespace Assignment8;

public class Selsort
{

    static void Main(string[] args)
    {
    }

    public static void SelectionSort(int[] arr) {
        for (int i = 0; i < arr.Length; i++) {
            int least = i;
            for (int j = i+1; j < arr.Length; j++)
                if (arr[j] < arr[least])
                    least = j;
            int tmp = arr[i]; arr[i] = arr[least]; arr[least] = tmp;
        }
    }
}