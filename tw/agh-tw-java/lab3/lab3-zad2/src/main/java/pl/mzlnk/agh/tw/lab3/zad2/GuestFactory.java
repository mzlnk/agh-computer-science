package pl.mzlnk.agh.tw.lab3.zad2;

import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;

import java.util.Random;

@RequiredArgsConstructor
public class GuestFactory {

    private static final Random r = new Random();

    private final Waiter waiter;

    @SneakyThrows
    public void startCreatingGuests(int maxPairs) {
        for(int i = 0; i < maxPairs; i++) {
            Guest g1 = new Guest(i, 0, waiter);
            Guest g2 = new Guest(i, initialDelay(), waiter);

            startGuest(g1);
            startGuest(g2);

            Thread.sleep(r.nextInt(1500) + 500);
        }
    }

    private void startGuest(Guest guest) {
        new Thread(guest).start();
    }

    private static long initialDelay() {
        return r.nextInt(5000) + 1500;
    }

}
