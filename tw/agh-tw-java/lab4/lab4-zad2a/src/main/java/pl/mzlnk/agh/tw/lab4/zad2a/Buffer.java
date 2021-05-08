package pl.mzlnk.agh.tw.lab4.zad2a;

import lombok.Getter;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;

import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

@Slf4j
public class Buffer {

    @Getter
    private final int size;
    private final Lock lock = new ReentrantLock();
    private final Condition condition = this.lock.newCondition();
    @Getter
    private boolean closed = false;

    private volatile int buffer = 0;

    public Buffer(int size) {
        this.size = size;
    }

    @SneakyThrows
    public void put(int size) {
        try {
            lock.lock();

            while (!canPut(size)) {
                condition.await();
            }

            this.buffer += size;
            // log.info("buffer: {}", this.buffer);

            condition.signalAll();
        } finally {
            lock.unlock();
        }
    }

    @SneakyThrows
    public void get(int size) {
        try {
            lock.lock();

            while (!canGet(size)) {
                condition.await();
            }

            this.buffer -= size;
            // log.info("buffer: {}", size);

            condition.signalAll();
        } finally {
            lock.unlock();
        }
    }

    public void closeBuffer() {
        this.closed = true;
    }

    private boolean canPut(int size) {
        return this.buffer + size <= this.size;
    }

    private boolean canGet(int size) {
        return this.buffer >= size;
    }

}
