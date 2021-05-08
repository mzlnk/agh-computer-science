package pl.mzlnk.agh.tw.lab3.zad2;

import java.util.Optional;

public class Table {

    private Guest guest1;
    private Guest guest2;

    public void addGuest(Guest guest) {
        if (this.guest1 == null) {
            this.guest1 = guest;
        } else {
            this.guest2 = guest;
        }
    }

    public void removeGuest(Guest guest) {
        if (this.guest1 != null && this.guest1.equals(guest)) {
            this.guest1 = null;
        } else {
            this.guest2 = null;
        }
    }

    public boolean isTableFree() {
        return guest1 == null && guest2 == null;
    }

    public boolean isSecondGuestOnTable(Guest guest) {
        return Optional.ofNullable(guest1)
                .map(Guest::getPairId)
                .map(id -> id == guest.getPairId())
                .orElse(false);
    }

}
