import java.util.*;
import java.io.*;
import java.net.*;

public class Main {
    public static void main(String[] args) {
     int[] arr = {3,1,4,1,5,9,6,2,5};
     MinHeap minHeap = new MinHeap(arr.length);

     for (int elem:arr){
         minHeap.insert(elem);
     }

     minHeap.printHeap();

     if (hej) System.out.println("heh");
    }

    private static boolean hej = true;

}


