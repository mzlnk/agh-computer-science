package pl.mzlnk.agh.tw.lab3.zad2;

public class Main {

    public static void main(String[] args) {
        Waiter waiter = new Waiter();
        GuestFactory guestFactory = new GuestFactory(waiter);

        guestFactory.startCreatingGuests(10);
    }

}
