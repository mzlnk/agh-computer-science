package pl.mzlnk.agh.tw.lab3.zad2;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;

import java.util.Random;

@Slf4j
@RequiredArgsConstructor
public class Guest implements Runnable {

    private static final Random r = new Random();

    @Getter
    private final int pairId;
    private final long initialDelay;
    private final Waiter waiter;

    @Override
    @SneakyThrows
    public void run() {
        Thread.sleep(initialDelay);

        log.info("[pair-{}] Guest arrived", pairId);
        waiter.askForTable(this);
        log.info("[pair-{}] Guest eating", pairId);
        Thread.sleep(r.nextInt(3000) + 1000);
        log.info("[pair-{}] Guest leaving", pairId);
        waiter.releaseTable(this);

    }
}
