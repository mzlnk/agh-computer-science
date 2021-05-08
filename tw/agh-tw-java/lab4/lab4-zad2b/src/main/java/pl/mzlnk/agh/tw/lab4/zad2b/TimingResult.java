package pl.mzlnk.agh.tw.lab4.zad2b;

import lombok.Getter;

@Getter
public class TimingResult {

    private long allTime;
    private int attempts;

    public void addTiming(long time) {
        this.allTime += time;
        this.attempts += 1;
    }

    public void addTiming(TimingResult timingResult) {
        this.allTime += timingResult.allTime;
        this.attempts += timingResult.attempts;
    }

    public double getAverage() {
        return 1D * allTime / attempts;
    }

    @Override
    public String toString() {
        return "" + this.getAverage();
    }
}
