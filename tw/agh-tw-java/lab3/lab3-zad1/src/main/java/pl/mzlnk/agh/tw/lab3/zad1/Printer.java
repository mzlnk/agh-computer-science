package pl.mzlnk.agh.tw.lab3.zad1;

import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;

import java.util.Random;

@RequiredArgsConstructor
public class Printer {

    private static final Random r = new Random();

    private final int id;

    @SneakyThrows
    public void printDocument(Document document) {
        Thread.sleep(r.nextInt(2000) + 500);
    }

    @Override
    public String toString() {
        return "printer-" + (this.id < 10 ? "0" : "") + this.id;
    }
}
