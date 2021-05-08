package pl.mzlnk.agh.tw.lab5.zad1;

import lombok.RequiredArgsConstructor;
import lombok.ToString;
import org.apache.commons.math3.stat.descriptive.moment.Mean;
import org.apache.commons.math3.stat.descriptive.moment.StandardDeviation;

public class TimeAnalyzer {

    public static TimeResults analyze(int attempts, Runnable runnable) {
        double[] values = new double[attempts];

        for(int i = 0; i < attempts; i++) {
            double before = System.currentTimeMillis();
            runnable.run();
            double after = System.currentTimeMillis();

            values[i] = (after - before);
        }

        return new TimeResults(
                attempts,
                new Mean().evaluate(values, 0, values.length),
                new StandardDeviation().evaluate(values)
        );
    }

    @RequiredArgsConstructor
    public static class TimeResults {

        public final int attempts;
        public final double averageTime;
        public final double standardDeviation;

        @Override
        public String toString() {
            return String.format(
                    "attempts: %d, averageTime: %f, standardDeviation: %f",
                    attempts, averageTime, standardDeviation
            );
        }
    }

}
