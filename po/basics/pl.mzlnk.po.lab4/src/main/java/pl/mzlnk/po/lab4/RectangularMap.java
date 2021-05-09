package pl.mzlnk.po.lab4;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Stream;

public class RectangularMap implements IWorldMap {

    private final Vector2D lowerLeft;
    private final Vector2D upperRight;

    private int width;
    private int height;

    private List<Animal> animals = new ArrayList<>();

    public RectangularMap(int width, int height) {
        this.width = width;
        this.height = height;

        this.lowerLeft = new Vector2D(0, 0);
        this.upperRight = new Vector2D(width - 1, height - 1);
    }

    @Override
    public boolean canMoveTo(Vector2D position) {
        return !isOccupied(position) && position.isInBounds(lowerLeft, upperRight);
    }

    @Override
    public boolean place(Animal animal) {
        if (this.isOccupied(animal.getPosition())) {
            return false;
        }
        return animals.add(animal);
    }

    @Override
    public void run(MoveDirection[] directions) {
        AtomicInteger i = new AtomicInteger(0);
        Stream.of(directions)
                .forEach(moveDirection -> {
                    animals.get(i.getAndIncrement() % animals.size()).move(moveDirection);
                });
    }

    @Override
    public boolean isOccupied(Vector2D position) {
        return animals
                .stream()
                .anyMatch(animal -> animal.getPosition().equals(position));
    }

    @Override
    public Object objectAt(Vector2D position) {
        return animals
                .stream()
                .filter(animal -> animal.getPosition().equals(position))
                .findAny()
                .orElse(null);
    }

    @Override
    public String toString() {
        return new MapVisualizer(this).draw(lowerLeft, upperRight);
    }

}
