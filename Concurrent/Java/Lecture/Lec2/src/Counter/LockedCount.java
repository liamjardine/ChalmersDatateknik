public class LockedCount {
    public static void main(String[] args) {
        LockedCounter counter = new LockedCounter();
        // threads t and u, sharing counter
        Thread t = new Thread(counter);
        Thread u = new Thread(counter);
        t.start(); // increment once
        u.start(); // increment twice
        try { // wait for t and u to terminate
            t.join();
            u.join();
        }
        catch (InterruptedException e)
        {
            System.out.println("Interrupted!");
        }
        // print final value of counter
        System.out.println(counter.counter());
    }
}
