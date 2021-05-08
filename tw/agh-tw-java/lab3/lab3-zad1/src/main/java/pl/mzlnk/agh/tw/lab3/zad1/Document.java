package pl.mzlnk.agh.tw.lab3.zad1;

import lombok.SneakyThrows;

import java.util.Random;

public class Document {

    private static final Random r = new Random();

    @SneakyThrows
    public void prepareForPrinting() {
        Thread.sleep(r.nextInt(3000) + 500L);
    }

}
