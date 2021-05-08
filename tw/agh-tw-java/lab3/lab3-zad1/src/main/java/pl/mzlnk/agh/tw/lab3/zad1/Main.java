package pl.mzlnk.agh.tw.lab3.zad1;

public class Main {

    public static void main(String[] args) {
        PrinterStorage printerStorage = new PrinterStorage(10);

        for(int i = 0; i < 50; i++) {
            new Thread(new PrinterTask(i, printerStorage)).start();
        }
    }

}
