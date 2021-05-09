package pl.mzlnk.po.lab5.map.impl;

import pl.mzlnk.po.lab5.dto.Vector2D;
import pl.mzlnk.po.lab5.enums.MoveDirection;
import pl.mzlnk.po.lab5.utils.Pair;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Stream;

public class RectangularMap extends AbstractBoundedWorldMap {

    private final Vector2D lowerLeft;
    private final Vector2D upperRight;

    public RectangularMap(int width, int height) {
        this.lowerLeft = new Vector2D(0, 0);
        this.upperRight = new Vector2D(width - 1, height - 1);
    }

    @Override
    public void run(MoveDirection[] directions) {
        AtomicInteger i = new AtomicInteger(0);
        Stream.of(directions)
                .forEach(moveDirection -> {
                    mapElements.get(i.getAndIncrement() % mapElements.size()).move(moveDirection);
                });
    }

    @Override
    protected boolean isInBounds(Vector2D position) {
        return position.isInBounds(lowerLeft, upperRight);
    }

    @Override
    protected Pair<Vector2D, Vector2D> getMapBounds() {
        return new Pair<>(lowerLeft, upperRight);
    }

}
