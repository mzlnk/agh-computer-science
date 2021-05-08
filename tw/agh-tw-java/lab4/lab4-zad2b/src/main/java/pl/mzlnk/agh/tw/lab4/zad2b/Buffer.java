package pl.mzlnk.agh.tw.lab4.zad2b;

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
    private final Condition consumerCondition = this.lock.newCondition();
    private final Condition producerCondition = this.lock.newCondition();

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
                producerCondition.await();

                if(closed) {
                    return;
                }
            }

            this.buffer += size;
            // log.info("buffer: {}", this.buffer);

            consumerCondition.signalAll();
        } finally {
            lock.unlock();
        }
    }

    @SneakyThrows
    public void get(int size) {
        try {
            lock.lock();

            while (!canGet(size)) {
                consumerCondition.await();

                if(closed) {
                    return;
                }
            }

            this.buffer -= size;
            // log.info("buffer: {}", size);

            producerCondition.signalAll();
        } finally {
            lock.unlock();
        }
    }

    public void closeBuffer() {
        try {
            lock.lock();

            this.closed = true;
            this.consumerCondition.signalAll();
            this.producerCondition.signalAll();
        } finally {
            lock.unlock();
        }

    }

    private boolean canPut(int size) {
        return this.buffer + size <= this.size;
    }

    private boolean canGet(int size) {
        return this.buffer >= size;
    }

}
