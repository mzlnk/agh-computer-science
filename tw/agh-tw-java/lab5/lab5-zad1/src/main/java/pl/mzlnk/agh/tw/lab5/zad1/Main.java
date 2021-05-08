package pl.mzlnk.agh.tw.lab5.zad1;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class Main {

    public static void main(String[] args) {
        MandelbrotProperties properties = MandelbrotProperties
                .builder()
                .width(800)
                .height(600)
                .maxIterations(2000)
                .zoom(250)
                .tasks(80)
                .threads(8)
                .build();

        var results = TimeAnalyzer.analyze(10, () -> Mandelbrot.create(properties));
        log.info("results: {}", results);
        MandelbrotFrame frame = new MandelbrotFrame(Mandelbrot.create(properties));
        frame.setVisible(true);
    }

    /**
     * results for:
     *
     * width:           1000
     * height:          500
     * maxIterations:   2000
     * zoom:            150
     * attempts:        10
     *
     *
     * Experiment 1:
     * - tasks: 50
     *
     *  +----------------------------+---------------------+----------------------+
     *  | configuration              | average time [ms]   | standard deviation   |
     *  +----------------------------+---------------------+----------------------+
     *  | 1 thread                   | 307.2               | 50.42                |
     *  +----------------------------+---------------------+----------------------+
     *  | threads = cores (4)        | 162.9               | 72.21                |
     *  +----------------------------+---------------------+----------------------+
     *  | threads = 2*cores (8)      | 167.4               | 47.36                |
     *  +----------------------------+---------------------+----------------------+
     *
     *
     * Experiment 2:
     * - threads: 8
     *
     *  +----------------------------+---------------------+----------------------+
     *  | configuration              | average time [ms]   | standard deviation   |
     *  +----------------------------+---------------------+----------------------+
     *  | tasks = threads (8)        | 186.5               | 56.26                |
     *  +----------------------------+---------------------+----------------------+
     *  | tasks = 10*threads (80)    | 163.3               | 38.72                |
     *  +----------------------------+---------------------+----------------------+
     *  | tasks = pixels (500 000)   | 635.3               | 175.97               |
     *  +----------------------------+---------------------+----------------------+
     *
     */

}
