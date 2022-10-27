/*
 * TDA384/DIT391 -- Principles of Concurrent Programming -- VT 2022
 *
 * ****** Java Tutorial ******
 *
 * Summary: the point of this tutorial is to
 *
 *  1. demonstrate how to program concurrently in Java (by example),
 *     e.g. we will see
 *
 *      - how to manage threads (define, create, start, join, ...),
 *      - how to use sync mechanisms (locks, monitors, semaphores, ...),
 *      - polling vs. waiting/blocking (if time permits),
 *
 *     details will follow in the lectures;
 *
 *  2. see common pitfalls when programming concurrently in Java, e.g.
 *
 *      - different examples of race conditions,
 *      - how (not) to use availablePermits() and tryAcquire(),
 *      - how (not) to combine different synchronization mechanisms,
 *      - how (not) to use Boolean flags (if time permits),
 *      - how (not) to use static fields (if time permits),
 *
 *     try to remember these in the labs!
 *
 */

import java.util.concurrent.Semaphore;
import java.util.concurrent.locks.*;


class Tutorial {
    public static void main(String[] args) {
        // Create a shared integer.
        BoxedInt sharedInt = new BoxedInt(0);

        // Create two counter instances.
        Counter c1 = new Counter(1, sharedInt);
        Counter c2 = new Counter(2, sharedInt);

        // Create two threads, both running a counter instance.
        Thread t1 = new Thread(c1);
        Thread t2 = new Thread(c2);

        // Start the two threads.
        t1.start();
        t2.start();

        // Wait for both threads to finish.
        try {
            t1.join();
            t2.join();
        } catch (InterruptedException e) {
            // Don't fail silently!
            // At least print the stack-trace associated with the exception.
            System.err.println("Whooops, unexpected interruption!");
            e.printStackTrace();
            System.exit(1); // Unexpected exception, terminate the program
        }

        System.out.println("OK, we're done.");
        System.out.println("The final counter value is " + sharedInt.getValue());
    }
}

class BoxedInt {
    int value;
    Lock lock;
    Semaphore sem;

    BoxedInt(int value) {
        this.value = value;
        lock = new ReentrantLock();
        sem = new Semaphore(1);  // the capacity of this semaphore is 1 initially.
    }

    // Version 1: DOES NOT WORK, RACE CONDITION!
  /*
  void inc() {
    ++value;
  }
  */

    // Version 2: using locks.
    void inc() {
        lock.lock();
        ++value;
        lock.unlock();
    }

    // Version 3: using BoxedInt as a monitor (synchronized locally).
  /*
  void inc() {
    synchronized(this) {
      ++value;
    }
  }
  */

    // Version 4: using BoxedInt as a monitor (synchronized method).
  /*
  synchronized void inc() {
    ++value;
  }
  */

    // Version 5: moving the synchronized block to Counter.run().
    // ** see code in Counter.run() **

    // Version 6: using a semaphore.
  /*
  void inc() {
    try {
      sem.acquire();
      // sem now has capacity 0
      ++value;
      sem.release();
      // sem has capacity 1 again until it is acquired by another thread.
    } catch (InterruptedException e) {
      e.printStackTrace();  // <-- don't fail silently!
      System.exit(1);       // <-- unexpected interruption, exit the program
    }
  }
  */

    /*********
     *
     * The following definitions are used in part B (batch locking)
     * which was not covered in the tutorial.  (See Counter.run() below
     * for some explanations.)
     *
     */

    // An un-synced increment function.  Not safe by itself...
    void incNoSync() {
        ++value;
    }

    // Methods to lock/unlock the boxed integer and do useful work while
    // waiting...

    // Part B, version 1: DOESN'T WORK, RACE CONDITION AND DEADLOCK!
  /*
  void acquireSem() {
    try {
      while (sem.availablePermits() != 0) {
        // Do useful work here...
        Thread.sleep(1);
      }

      sem.acquire();
    } catch (InterruptedException e) {
      e.printStackTrace();  // <-- don't fail silently!
      System.exit(1);       // <-- unexpected interruption, exit the program
    }
  }
  */

    // Part B, version 2: use tryAcquire()
  /*
  void acquireSem() {
    try {
      while (!sem.tryAcquire()) {
        // Do useful work here
        //
        // NOTE: the 'sleep()' call is just a substitute for actual,
        // useful work.  Dont use 'sleep()' in real programs!
        Thread.sleep(1);
      }
    } catch (InterruptedException e) {
      e.printStackTrace();  // <-- don't fail silently!
      System.exit(1);       // <-- unexpected interruption, exit the program
    }
  }
  */

    void releaseSem() {
        sem.release();
    }

    // Return the value of this BoxedInt instance.
    int getValue() {
        return value;
    }
}

class Counter implements Runnable {
    int tid;
    BoxedInt counter;
    static final int ITERATIONS = 10000;

    Counter(int tid, BoxedInt counter) {
        this.tid = tid;
        this.counter = counter;
    }

    public void run() {
        System.out.println("Hello world, I am thread " + tid);

        // Part A: locking for each invocation of inc()
        for (int i = 0; i < ITERATIONS; ++i) {
            counter.inc();
        }

        // Part A, version 5a: synchronized at the call site.
    /*
    for (int i = 0; i < ITERATIONS; ++i) {
      synchronized (counter) {
        counter.incNoSync();
      }
    }
    */

        // Part A, version 5b: DOES NOT WORK, sync on WRONG MONITOR!!!
    /*
    for (int i = 0; i < ITERATIONS; ++i) {
      synchronized (this) {  // <-- synchronizing on 'this' doesn't work here
        counter.incNoSync();
      }
    }
    */

        // Part A, version 5c: batch synchronized at the call site.
    /*
    synchronized (counter) {
      for (int i = 0; i < ITERATIONS; ++i) {
        counter.incNoSync();
      }
    }
    */

        // Part B: batch locking.

        // Instead of locking and unlocking individual calls to inc(), we
        // may want to lock the shared integer for the entire duration of
        // the 'for' loop.

    /*
    counter.acquireSem();
    for (int i = 0; i < ITERATIONS; ++i) {
      counter.incNoSync();
    }
    counter.releaseSem();
    */
    }
}

/*
 * Alternatively, we could have declared the class Counter as follows:
 *

class Counter extends Thread { ... }

 * and instantiated them as

Thread t1 = new Counter(...);
Thread t2 = new Counter(...);

 * but using a separate Runnable is more flexible because it allows us
 * to use (multiple) inheritance, etc.
 *
 */