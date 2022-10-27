import java.util.Arrays;

public class Quicksort {

    public static void main (String[] args){
        int[] arr = {3,6,5,8,3,99,4,6,876,23,63,745,8567};
        int[] arr1 = {5,2,6,4,1,3};

        System.out.println(Arrays.toString(arr));
        quickSort(arr, 0, arr.length-1);
        System.out.println(Arrays.toString(arr));

    }


    static int partition(int[] arr, int low, int high)
    {

        int pivot = arr[high];
        int i = (low - 1);

        for(int j = low; j <= high - 1; j++) {
            if (arr[j] < pivot) {
                i++;
                swap(arr, i, j);
            }
        }
        swap(arr, i + 1, high);
        return (i + 1);
    }


    static void quickSort(int[] arr, int low, int high)
    {
        if (low < high){

            int pi = partition(arr, low, high);

            quickSort(arr, low, pi - 1);
            quickSort(arr, pi + 1, high);
        }
    }

    public static int[] swap(int[] arr, int a, int b){
        int store = arr[b];
        arr[b] = arr[a];
        arr[a] = store;
        return arr;
    }

}
