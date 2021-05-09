package pl.mzlnk.po.lab7.map.element;

import pl.mzlnk.po.lab7.enums.MapDirection;
import pl.mzlnk.po.lab7.enums.MoveDirection;
import pl.mzlnk.po.lab7.map.PositionChangedObserver;
import pl.mzlnk.po.lab7.map.WorldMap;

import java.util.ArrayList;
import java.util.List;

public class Animal extends BaseMapElement {

    private static final Vector2D INIT_POSITION = new Vector2D(2, 2);
    private static final MapDirection INIT_DIRECTION = MapDirection.NORTH;

    private PositionChangedObserver observer;
    private MapDirection mapDirection;

    public Animal(WorldMap map) {
        this(map, INIT_POSITION);
    }

    public Animal(WorldMap map, Vector2D initPosition) {
        this(map, initPosition, INIT_DIRECTION);
    }

    public Animal(WorldMap map, Vector2D initPosition, MapDirection initDirection) {
        super(map, initPosition);
        this.mapDirection = initDirection;
    }

    @Override
    public void move(MoveDirection direction) {
        switch (direction) {
            case LEFT -> this.mapDirection = mapDirection.previous();
            case RIGHT -> this.mapDirection = mapDirection.next();
            case FORWARD -> this.move(mapDirection.toUnitVector());
            case BACKWARD -> this.move(mapDirection.toUnitVector().opposite());
        }
    }

    @Override
    public String toString() {
        return mapDirection.toString();
    }

    @Override
    public boolean equals(Object other) {
        if (!(other instanceof Animal)) {
            return false;
        }

        Animal animal = (Animal) other;
        return this.position.equals(animal.position) &&
                this.mapDirection.equals(animal.mapDirection) &&
                this.map.equals(animal.map);
    }

    public void addObserver(PositionChangedObserver observer) {
        this.observer = observer;
    }

    public void removeObserver(PositionChangedObserver observer) {
        this.observer = null;
    }

    private void move(Vector2D v) {
        Vector2D newPos = this.position.add(v);
        if (map.canMoveTo(newPos) || map.objectAt(newPos).map(e -> e instanceof Grass).orElse(false)) {
            map.objectAt(newPos).ifPresent(map::remove);
            if(observer != null) {
                observer.positionChanged(this.position, newPos);
            }
            this.position = newPos;
        }
    }

}
