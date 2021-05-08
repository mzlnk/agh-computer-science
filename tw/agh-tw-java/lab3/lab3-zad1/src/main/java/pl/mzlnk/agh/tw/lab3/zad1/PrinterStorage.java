package pl.mzlnk.agh.tw.lab3.zad1;

import lombok.SneakyThrows;

import java.util.ArrayDeque;
import java.util.Queue;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class PrinterStorage {

    private final Lock lock = new ReentrantLock();
    private final Condition condition = lock.newCondition();

    private Queue<Printer> availablePrinters = new ArrayDeque<>();

    public PrinterStorage(int printers) {
        for(int i = 0; i < printers; i++) {
            this.availablePrinters.offer(new Printer(i));
        }
    }

    @SneakyThrows
    public Printer takeAvailablePrinter() {
        try {
            lock.lock();

            while(availablePrinters.isEmpty()) {
                condition.await();
            }

            return availablePrinters.poll();
        } finally {
            lock.unlock();
        }
    }
    public void releasePrinter(Printer printer) {
        lock.lock();

        availablePrinters.offer(printer);
        condition.signalAll();

        lock.unlock();
    }

}
