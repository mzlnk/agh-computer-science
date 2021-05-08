package pl.mzlnk.agh.tw.lab5.zad1;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;

import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

@Slf4j
@RequiredArgsConstructor
public class Mandelbrot {

    @Getter
    private final int width;
    @Getter
    private final int height;

    private final double zoom;
    private final int maxIterations;

    private final int threads;
    private final int tasks;

    @Getter
    private BufferedImage image;

    public static Mandelbrot create(MandelbrotProperties properties) {
        Mandelbrot mandelbrot = new Mandelbrot(
                properties.getWidth(),
                properties.getHeight(),
                properties.getZoom(),
                properties.getMaxIterations(),
                properties.getThreads(),
                properties.getTasks()
        );

        mandelbrot.generateImage();

        return mandelbrot;
    }

    @SneakyThrows
    private void generateImage() {
        this.image = new BufferedImage(this.width, this.height, BufferedImage.TYPE_INT_RGB);

        final ExecutorService pool = Executors.newFixedThreadPool(this.threads);
        final List<Future<List<Point>>> tasks = new ArrayList<>();

        final int allPoints = this.width * this.height;
        final int pointsPerTask = allPoints / this.tasks;

        for (int point = 0; point < allPoints; point += pointsPerTask) {
            List<Integer> pointsForTask = IntStream
                    .range(point, Math.min(point + pointsPerTask, allPoints))
                    .boxed()
                    .collect(Collectors.toList());

            var task = new MandelbrotTask(pointsForTask);
            tasks.add(pool.submit(task));
        }

        // log.info("all tasks: {}, allPoints: {}, pointsPerTask: {}", tasks.size(), allPoints, pointsPerTask);

        for (var task : tasks) {
            List<Point> points = task.get();
            // log.info("task results:");
            points.forEach(point -> {
                final int x = point.pos / height;
                final int y = point.pos % height;

                // log.info("[{}] -> ({}, {}) -> {}", point.pos, x, y, point.rgb);
                this.image.setRGB(x, y, point.rgb);
            });
        }
    }

    @RequiredArgsConstructor
    private static final class Point {

        public final int pos;
        public final int rgb;

    }

    @RequiredArgsConstructor
    private final class MandelbrotTask implements Callable<List<Point>> {

        private final List<Integer> points;

        private double zx, zy, cX, cY, tmp;

        @Override
        public List<Point> call() {
            return this.points
                    .stream()
                    .map(point -> {
                        final int x = point / height;
                        final int y = point % height;

                        zx = zy = 0;
                        cX = (x - 0.5 * width) / zoom;
                        cY = (y - 0.5 * height) / zoom;
                        int iter = maxIterations;
                        while (zx * zx + zy * zy < 4 && iter > 0) {
                            tmp = zx * zx - zy * zy + cX;
                            zy = 2.0 * zx * zy + cY;
                            zx = tmp;
                            iter--;
                        }

                        return new Point(point, iter | (iter << 8));
                    })
                    .collect(Collectors.toList());
        }
    }

}