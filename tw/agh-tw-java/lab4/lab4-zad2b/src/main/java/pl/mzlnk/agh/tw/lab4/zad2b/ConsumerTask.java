package pl.mzlnk.agh.tw.lab4.zad2b;

public class ConsumerTask extends AbstractProcessorTask {

    public ConsumerTask(Buffer buffer) {
        super(buffer);
    }

    @Override
    protected int process(Buffer buffer) {
        int size = r.nextInt(buffer.getSize() / 2 - 1) + 1;
        buffer.get(size);

        return size;
    }

}
