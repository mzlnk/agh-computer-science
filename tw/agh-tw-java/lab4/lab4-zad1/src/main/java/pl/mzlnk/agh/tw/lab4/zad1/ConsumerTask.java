package pl.mzlnk.agh.tw.lab4.zad1;

public class ConsumerTask extends AbstractProcessorTask {

    public ConsumerTask(Buffer buffer, int from, int minSleepTime) {
        super(buffer, from, 0, minSleepTime);
    }

}
