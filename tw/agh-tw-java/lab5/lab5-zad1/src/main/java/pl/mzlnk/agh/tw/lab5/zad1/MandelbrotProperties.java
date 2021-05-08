package pl.mzlnk.agh.tw.lab5.zad1;

import lombok.Builder;
import lombok.Getter;

@Getter
@Builder
public class MandelbrotProperties {

    private final int width;
    private final int height;

    private final int maxIterations;
    private final double zoom;

    private final int threads;
    private final int tasks;

}
