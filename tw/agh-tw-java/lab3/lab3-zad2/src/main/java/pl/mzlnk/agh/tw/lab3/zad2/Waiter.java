package pl.mzlnk.agh.tw.lab3.zad2;

import lombok.SneakyThrows;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class Waiter {

    private final Lock lock = new ReentrantLock();
    private final Condition condition = lock.newCondition();

    private final List<Guest> guests = new ArrayList<>();

    private final Table table = new Table();

    @SneakyThrows
    public void askForTable(Guest guest) {
        try {
            lock.lock();

            guests.add(guest);

            while(!table.isTableFree() || !isPairAvailable(guest.getPairId())) {
                if(table.isSecondGuestOnTable(guest)) {
                    break;
                }
                condition.await();
            }

            table.addGuest(guest);
            guests.remove(guest);

            condition.signalAll();
        } finally {
            lock.unlock();
        }
    }

    public void releaseTable(Guest guest) {
        lock.lock();

        table.removeGuest(guest);
        condition.signalAll();

        lock.unlock();
    }

    private boolean isPairAvailable(int pairId) {
        return guests
                .stream()
                .filter(g -> g.getPairId() == pairId)
                .count() == 2;
    }

}
