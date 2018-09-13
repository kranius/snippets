import java.util.concurrent.locks.LockSupport;

public class ThreadRing {
  static final int THREAD_COUNT = 503;

  public static class MessageThread extends Thread {
    MessageThread nextThread;
    volatile Integer message;

    public MessageThread(MessageThread nextThread, int name) {
      super(""+name);
      this.nextThread = nextThread;
    }

    public void run() {
      while (true)
        nextThread.enqueue(dequeue());
    }

    public void enqueue(Integer hopsRemaining) {
      if (hopsRemaining == 0){
        System.out.println(getName());
        System.exit(0);
      }
      message = hopsRemaining - 1;
      LockSupport.unpark(this);
    }

    private Integer dequeue() {
      while (message == null) {
        LockSupport.park();
      }
      Integer msg = message;
      message = null;
      return msg;
    }
  }

  public static void main(String args[]) throws Exception {
    int hopCount = Integer.parseInt(args[0]);

    MessageThread first = null;
    MessageThread last = null;
    for (int i=THREAD_COUNT; i>=1; i--) {
      first = new MessageThread(first, i);
      if (i == THREAD_COUNT)
        last = first;
    }
    last.nextThread = first;

    MessageThread t = first;
    do {
      t.start();
      t = t.nextThread;
    } while (t != first);
    first.enqueue(hopCount);
    first.join();
  }
}
