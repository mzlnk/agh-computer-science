package pl.mzlnk.agh.tw.lab4.zad2a;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.Callable;

@Slf4j
@RequiredArgsConstructor
public abstract class AbstractProcessorTask implements Callable<Map<Integer, TimingResult>> {

    protected static final Random r = new Random();

    private final Buffer buffer;

    private Map<Integer, TimingResult> timings = new HashMap<>();

    protected abstract int process(Buffer buffer);

    @Override
    public final Map<Integer, TimingResult> call() {
        while (!buffer.isClosed()) {
            long before = System.nanoTime();
            int size = this.process(this.buffer);
            long after = System.nanoTime();

            timings.putIfAbsent(size, new TimingResult());
            timings.compute(size, (s, r) -> {
                r.addTiming(after - before);
                return r;
            });
        }

        return timings;
    }
}
