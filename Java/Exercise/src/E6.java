import java.util.PriorityQueue;

public class E6 {


        public static void main(String[] args){
            System.out.println("Hello world");

            int[] list = {3,1,9,1,5,9,2,6};
            nLargestElements(3,list);

        }
        /////////////////////////// 3

        public static void nLargestElements(int n, int[] list){
            PriorityQueue<Integer> pq = new PriorityQueue<>();
            for (int i = 0; i < list.length; i++){
                pq.add(list[i]);
                if (pq.size() > n)
                    pq.remove();
            }

            while (!pq.isEmpty())
                pq.remove();
            System.out.println(pq.toString());
        }




}

