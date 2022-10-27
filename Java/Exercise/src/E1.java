import java.util.*;
import java.io.*;

public class E1 {
    public static void main(String[] args){

        int[] arr1 = {5,4,3,2,1};
        int[] arr2 = {1,2,2,2,3,3,4,4,4,4};
        int[] arr3 = {1,4,2,6,3,2,2,6,8,1,9,0};
        int[] arr4 = {0};
        int[] arr5 = {1,3,5,7,9};
        int[] arr6 = {2,4,6,8,10};
        int[] arr7 = {2,6,3,8,5,2,1,3,9,4,7};

        //System.out.println(Arrays.toString(arr2));
        //System.out.println(arr1[0]);
        //System.out.println(binarySearchIt(arr2, 4));
        //System.out.println(binarySearchRe(arr2, 1,0,2));
        System.out.println(integerSquareRoot(9));
        System.out.println(integerSquareRootLec(9));
        //System.out.println(minimum(arr3));
        //System.out.println(Arrays.toString(insert(arr4, 3)));
        //System.out.println(Arrays.toString(merge(arr5, arr6)));
        //System.out.println(Arrays.toString(mergeL(arr7)));
        //System.out.println(mode(arr2));


    }

    /////////////////////////// 1. Binary search Iterative

    public static int binarySearchIt(int[] arr, int item){
        int lo = 0;
        int hi = arr.length - 1;

        while(lo <= hi){
            int mid = (lo+hi)/2;

            if (item < arr[mid])
                hi = mid;
            else if (item > arr[mid])
                lo = mid;
            else
                return mid;
        }
        return -1;
    }

    /////////////////////////// 2. Binary search Recursive

    public static int binarySearchRe(int[] arr, int item, int lo, int hi){
        int mid = (lo+hi)/2;
        if (item < arr[mid])
            binarySearchRe(arr,item,lo,mid);
        else if (item > arr[mid])
            binarySearchRe(arr,item,mid,hi);
        else if (item == arr[mid])
            return mid;
        return -1;
    }

    /////////////////////////// 3. Integer Square Root

    // Complexity O(sqrt(n))
    public static int integerSquareRoot(int n){
        int m = 0;
        while (m*m <= n){
            m++;
        }
        return m-1;
    }

    // Complexity O(log(n))
    public static int integerSquareRootLec(int n){
        int begin = 1;
        int end = n;
        int result = 0;
        while (begin <= end){
            int mid = (begin + end)/2;
            if (mid*mid == n)
                return mid;
            else if (mid*mid < n) {
                begin = mid + 1;
                result = mid;
            }
            else end = mid - 1;
        }

        return result;
    }

    /////////////////////////// 5. Minimum, insert and merge

    public static int minimum(int[] arr){
        int min = arr[0];
        for (int i = 1; i < arr.length; i++){
            if (min > arr[i])
                min = arr[i];
        }
        return min;
    }

    public static int[] insert(int[] arr, int x){
        int[] arrNew = new int[arr.length];
        int j = 0;
        if (arr.length > 2) {
            for (int i = 0; i < arr.length-1; i++) {
                arrNew[i] = arr[j];
                if (arr[i]<=x & x<=arr[i+1]){
                    arrNew[i+1] = x;
                    i++;
                }
                j++;
            }
            arrNew[arrNew.length-1]=arr[arr.length-2];
        }
        else {
            if (x > arr[0]) {
                arr[1] = x;
                arrNew = arr;
            } else {
                arrNew[0] = x;
                arrNew[1] = arr[0];
            }
        }

        return arrNew;
    }

    public static int[] merge(int[] arr1, int[] arr2) {

        int[] arrNew = new int[arr1.length + arr2.length];
        int i = 0;
        int j = 0;
        for (int k = 0; k < arrNew.length; k++) {
            if (i == arr1.length){
                arrNew = filler(arrNew, arr2, k, j);
                break;
            }
            else if (j == arr2.length){
                arrNew = filler(arrNew, arr1, k, i);
                break;
            }
            else {
                if (arr1[i] <= arr2[j]) {
                    arrNew[k] = arr1[i];
                    i++;
                } else if (arr1[i] > arr2[j]) {
                    arrNew[k] = arr2[j];
                    j++;
                }
            }
        }
        return arrNew;
    }

    public static int[] filler(int[] arr, int[] filling, int index, int fillingIndex){
        for (int i = index; i < arr.length; i++){
            arr[i] = filling[fillingIndex];
            fillingIndex++;
        }
        return arr;
    }

    /////////////////////////// From lecture

    public static final int[] mergeL(int[] array){
        return sortL(array, 0, array.length-1);
    }

    public static final int[] sortL(int[] array, int lo, int hi){
        if ( lo == hi) {
            int element = array[lo];
            int[] result = {element};
            return result;
        }
        if (lo > hi){
            int[] result = {};
            return result;
        }

        int mid = (lo+hi)/2;
        int[] sorted1 = sortL(array, lo, mid);
        int[] sorted2 = sortL(array, mid, hi);
        return merge(sorted1, sorted2);
    }

    /////////////////////////// Bonus: A program that finds the mode given an array

    public static int mode(int[] arr){
        //sort(arr);
        int mode = -1;
        int prevMode = 0;
        int arrMode = 0;


        for (int i = 0; i < arr.length-1; i++){
            if (arr[i] == arr[i+1])
                arrMode ++;
            else if (arrMode > prevMode){
                    mode = arr[i];
                    prevMode = arrMode;
                    arrMode = 0;
                }
            }
        return mode;
    }


}

