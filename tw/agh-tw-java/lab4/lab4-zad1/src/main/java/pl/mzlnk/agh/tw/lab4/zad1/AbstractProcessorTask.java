package pl.mzlnk.agh.tw.lab4.zad1;

import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;

import java.util.Random;

@RequiredArgsConstructor
public abstract class AbstractProcessorTask implements Runnable {

    private static final Random r = new Random();

    private final Buffer buffer;
    private final int from;
    private final int to;
    private final long minSleepTime;

    private int currentPos = 0;

    @Override
    @SneakyThrows
    public final void run() {
        while(true) {
            this.buffer.setValueAt(this.currentPos, this.from, this.to);
            this.currentPos = (this.currentPos + 1) % this.buffer.getSize();

            Thread.sleep(minSleepTime + r.nextInt(1000));
        }
    }

}
