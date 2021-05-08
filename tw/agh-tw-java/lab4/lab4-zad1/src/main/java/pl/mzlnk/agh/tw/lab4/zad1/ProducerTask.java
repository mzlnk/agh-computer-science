package pl.mzlnk.agh.tw.lab4.zad1;

public class ProducerTask extends AbstractProcessorTask {

    public ProducerTask(Buffer buffer, int minSleepTime) {
        super(buffer, 0, 1, minSleepTime);
    }

}
