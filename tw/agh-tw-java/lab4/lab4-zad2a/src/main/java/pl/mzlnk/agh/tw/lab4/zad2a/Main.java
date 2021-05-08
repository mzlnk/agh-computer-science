package pl.mzlnk.agh.tw.lab4.zad2a;

import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.FutureTask;

@Slf4j
public class Main {

    @SneakyThrows
    public static void main(String[] args) {
        final Buffer buffer = new Buffer(100000);
        final int producers = 1000;
        final int consumers = 1000;

        final ExecutorService service = Executors.newFixedThreadPool(producers + consumers);

        final List<FutureTask<Map<Integer, TimingResult>>> producerTasks = new ArrayList<>();
        final List<FutureTask<Map<Integer, TimingResult>>> consumerTasks = new ArrayList<>();

        log.info("buffer opened");

        for (int i = 0; i < producers; i++) {
            var task = new FutureTask<>(new ProducerTask(buffer));

            producerTasks.add(task);
            service.submit(task);
        }

        for (int i = 0; i < consumers; i++) {
            var task = new FutureTask<>(new ConsumerTask(buffer));

            consumerTasks.add(task);
            service.submit(task);
        }

        Thread.sleep(5000);
        buffer.closeBuffer();

        log.info("buffer closed");

        Map<Integer, TimingResult> pResults = new HashMap<>();
        Map<Integer, TimingResult> cResults = new HashMap<>();

        for (var task : producerTasks) {
            var result = task.get();
            result.forEach((key, value) -> {
                pResults.putIfAbsent(key, new TimingResult());
                pResults.compute(key, (k, v) -> {
                    v.addTiming(value);
                    return v;
                });
            });
        }

        for (var task : consumerTasks) {
            var result = task.get();
            result.forEach((key, value) -> {
                cResults.putIfAbsent(key, new TimingResult());
                cResults.compute(key, (k, v) -> {
                    v.addTiming(value);
                    return v;
                });
            });
        }

        ResultsExporter.exportResults(pResults, cResults, "m100k-p1k-k1k");

        log.info("Producers:");
        pResults.forEach((key, value) -> log.info("[PUT] size {} -> {} ns", key, value));

        log.info("Consumers:");
        cResults.forEach((key, value) -> log.info("[GET] size {} -> {} ns", key, value));

        System.exit(0);
    }

}
