import java.util.concurrent.locks.*;

public class LockedCounter extends CCounter
{
    @Override
    public void run() {
        lock.lock();
        try  {
            // int cnt = counter;
            // counter = counter + 1;
            super.run();
        } finally {
            lock.unlock();
        }
    }
    // shared by all threads working on this object
    private Lock lock = new ReentrantLock();
}
