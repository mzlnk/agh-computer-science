package pl.mzlnk.agh.tw.lab3.zad1;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@RequiredArgsConstructor
public class PrinterTask implements Runnable {

    private final int id;
    private final PrinterStorage printerStorage;

    public void run() {
        while(true) {
            Document document = new Document();
            document.prepareForPrinting();

            Printer printer = printerStorage.takeAvailablePrinter();

            log.info("[{}][task-{}] Started printing", printer, id);
            printer.printDocument(document);
            log.info("[{}][task-{}] Finished printing", printer, id);

            printerStorage.releasePrinter(printer);
        }
    }
}
