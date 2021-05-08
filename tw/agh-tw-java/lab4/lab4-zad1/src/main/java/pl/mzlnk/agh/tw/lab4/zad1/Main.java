package pl.mzlnk.agh.tw.lab4.zad1;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class Main {

    public static void main(String[] args) {
        Buffer buffer = new Buffer(10);
        log.info("buffer: {}", buffer);

        final int processorTasks = 5;

        new Thread(new ProducerTask(buffer, 1000)).start();
        new Thread(new ConsumerTask(buffer, 6, 1000)).start();

        for(int i = 0; i < processorTasks; i++) {
            new Thread(new ProcessorTask(buffer, i + 1, i + 2, 2000)).start();
        }
    }

}
