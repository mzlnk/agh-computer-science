package pl.mzlnk.po.lab4;

public class Animal {

    private static final Vector2D INIT_POSITION = new Vector2D(2, 2);
    private static final MapDirection INIT_DIRECTION = MapDirection.NORTH;

    private MapDirection mapDirection;
    private Vector2D position;

    private IWorldMap map;

    public Animal(IWorldMap map) {
        this(map, INIT_POSITION);
    }

    public Animal(IWorldMap map, Vector2D initPosition) {
        this(map, initPosition, INIT_DIRECTION);
    }

    public Animal(IWorldMap map, Vector2D initPosition, MapDirection initDirection) {
        this.map = map;
        this.position = initPosition;
        this.mapDirection = initDirection;
    }

    public Vector2D getPosition() {
        return position;
    }

    public void move(MoveDirection direction) {
        switch (direction) {
            case LEFT -> this.mapDirection = mapDirection.previous();
            case RIGHT -> this.mapDirection = mapDirection.next();
            case FORWARD -> this.move(mapDirection.toUnitVector());
            case BACKWARD -> this.move(mapDirection.toUnitVector().opposite());
        }
    }

    private void move(Vector2D v) {
        Vector2D newPos = this.position.add(v);
        this.position = map.canMoveTo(newPos) ? newPos : position;
    }

    @Override
    public boolean equals(Object other) {
        if(!(other instanceof Animal)) {
            return false;
        }

        Animal animal = (Animal) other;
        return this.position.equals(animal.position) &&
                this.mapDirection.equals(animal.mapDirection) &&
                this.map.equals(animal.map);
    }

    @Override
    public String toString() {
        return mapDirection.toString();
    }

}
