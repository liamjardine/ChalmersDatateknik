import java.util.*;
import java.io.*;

public class E3 {

    public static void main (String[] args){

        int[] arr1 = {1,2,3,4,5,6};
        int[] arr2 = {100,101};
        System.out.println(Arrays.toString(mergeSortedArrays(arr1, arr2)));
    }

    /////////////////////////// 2. Adding operations to dynamic arrays
    /*
    * 1. Deleting the last element can be done by setting the element to null.
    * 2. If there is no resizing the capacity will not adjust.
    * Leaving us with a big overhead.
    * 3. Worst case for adjusting a dynamic arrays capacity could look like:
    * - Starting with an empty array: Add n elements.
    * - Double size, n*2.
    * - Delete n elements.
    * - Resize, n*2.
    * 4.
    */

    /////////////////////////// 3. Find duplicates
    /*
    * If you are searching for duplicates in a sorted array, you only need to look
    * at the element i and i+1 and compare, if equal return true. Else i++ and repeat.
    *
    * This will be linear since we do need to go through the list more than once, i.e.
    * one loop is sufficient.
    */

    /////////////////////////// 4.
    /*
    * 1. O(n) - Finding an element in an unsorted array of n elements.
    * Traversing through a for loop once.
    *
    * 2. O(log n) - Finding an element in a sorted array of n elements.
    * We can use binary search by looking at the midmost element and divide it until
    * we find our key.
    *
    * 3. O(1) - Adding an element to an unsorted array of n elements,
    * assuming there is an empty space at the end of the array.
    * We simply just add it at the end, and this will also be constant time.
    *
    * 4. O(n) - Adding an element to a sorted array of n elements,
    * assuming there is an empty space at the end of the array
    * (the resulting array should also be sorted).
    * We first need to do a binary search and find where you
    * should place the element. Then you need make room in the array
    * by moving all elements, traversing through and that operation is linear.
    */

    /////////////////////////// 5.
    /*
    * Worst case complexity is O(n log n) since we need to go through
    * n elements and for every element choose on the two elements to sort
    */
    public static int[] mergeSortedArrays(int[] arr1, int[] arr2){
        int[] resultArray = new int[arr1.length+arr2.length];
        int j = 0;
        int k = 0;
        for (int i = 0; i < resultArray.length; i++){
            if (j == arr1.length)
                return filler(resultArray, arr2, i, k);
            else if (k == arr2.length)
                return filler(resultArray, arr1, i, j);
            else {
                if (arr1[j]<arr2[k]){
                    resultArray[i] = arr1[j];
                    j++;}
                else if (arr1[j]>=arr2[k]){
                    resultArray[i] = arr2[k];
                    k++;}
            }
        }
        return resultArray;}

    public static int[] filler (int[] arrFilled, int[] filling, int indexResult, int indexFilling){
        for(int i = indexResult; i < arrFilled.length; i++){
            arrFilled[i] = filling[indexFilling];
            indexFilling++;}
        return arrFilled;}

    /////////////////////////// 6.
    /*
    * Algorithm     Worst-case    Average-case
    *
    * Insertion     O(n^2)        O(n^2)
    * Merge         O(n log n)    O(n log n)
    * Quick         O(n^2)        O(n log n)
    * */

    /////////////////////////// 7.
    /*
    * 3n^3 + 2n^2 + 199                 Order of growth: n^3
    * 3n^3 – 2n^2 – 199                 Order of growth: n^3
    * 3n^3 · 2n^2 · 199                 Order of growth: n^5
    * 3n^3 / 2n^2 / 199                 Order of growth: n
    * (n + 2n^2)(2n + 3n^2)             Order of growth: n^4
    * (n + 2n)(2 log_2 n + 3 log_2 n)   Order of growth: n log n
    */

    /////////////////////////// 8.

    /*
    * f1: O(n) - Linear
    * f2: O(n^2)
    * f3: O((n+1)n/2) -> O(n^2)
    * f4: n^3/30 -> O(n^3)
    * f5: O(2*n) -> O(n)
    * f6: n^2+n -> O(n^2)
    * f7: (n/2)(n/c) -> O(n^2)
    * f8: O(n^(log n)) -> O(n)
    * f9: O(n^2 * log n)
    */

    /////////////////////////// B1.

    /*
    * Deleting an arbitrary element -> O(n)
    */

    /////////////////////////// 2.

    /*
    * O(10n^10 + 5n^5 + 2n^2 + 1) -> O(n^10)
    * O(1/n^2 + 1/n + 1) -> O(log n)
    * O((5n + 2)(7 – 3/n)(n + 1)) -> O(n^2)
    * O(log n^2 / log n) -> O(log n)
    * O(log (n^2 + 1) / log n) -> O(log n)
    * O(1 + n^100 / 2^n) -> O(log n)
    * O(log (log n) + log n) -> O(log n)
    */


    /////////////////////////// Q5.

    /*
    * What is the worst-case asymptotic complexity in n of adding
    * a single element to a dynamic array of size n?
    *
    *  - If the array is full we need to make a new array
    * of some capacity. We then copy all elements in the old array.
    * The insert the new element. Worst-case here will be the copying, O(n).
    *
    * What is the average asymptotic complexity in n of adding a single
    * element to a dynamic array of size n?
    *
    * - Most of the time we don't need to increase the capacity.
    * We just add one, O(1).
    *
    *
    */



}
