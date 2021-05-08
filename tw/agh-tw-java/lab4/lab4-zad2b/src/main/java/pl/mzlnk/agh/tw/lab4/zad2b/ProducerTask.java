package pl.mzlnk.agh.tw.lab4.zad2b;

public class ProducerTask extends AbstractProcessorTask {

    public ProducerTask(Buffer buffer) {
        super(buffer);
    }

    @Override
    protected int process(Buffer buffer) {
        int size = r.nextInt(buffer.getSize() / 2 - 1) + 1;
        buffer.put(size);

        return size;
    }

}
