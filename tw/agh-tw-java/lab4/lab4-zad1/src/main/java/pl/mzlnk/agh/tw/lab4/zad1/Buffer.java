package pl.mzlnk.agh.tw.lab4.zad1;

import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;

import java.util.Arrays;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

@Slf4j
public class Buffer {

    private final int size;
    private final int[] buffer;

    private Lock lock = new ReentrantLock();
    private Condition condition = lock.newCondition();

    public Buffer(int size) {
        this.size = size;
        this.buffer = new int[size];
    }

    public int getSize() {
        return size;
    }

    @SneakyThrows
    public void setValueAt(int pos, int from, int to) {
        try {
            lock.lock();

            while(this.buffer[pos] != from) {
                condition.await();
            }

            this.buffer[pos] = to;
            log.info("buffer: {}", this.buffer);

            condition.signalAll();
        } finally {
            lock.unlock();
        }
    }

    @Override
    public String toString() {
        return Arrays.toString(buffer);
    }

}
