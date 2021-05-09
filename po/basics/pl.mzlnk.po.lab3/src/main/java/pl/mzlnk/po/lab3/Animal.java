package pl.mzlnk.po.lab3;

public class Animal {

    private static final String TOSTRING_PATTERN = "Położenie: {position}, kierunek: {direction}";

    private static final Vector2D lowerLeft = new Vector2D(0, 0);
    private static final Vector2D upperRight = new Vector2D(4, 4);

    private Vector2D position = new Vector2D(2,2 );
    private MapDirection mapDirection = MapDirection.NORTH;

    public Vector2D getPosition() {
        return position;
    }

    public MapDirection getMapDirection() {
        return mapDirection;
    }

    private void setMapDirection(MapDirection mapDirection) {
        this.mapDirection = mapDirection;
    }

    public void move(MoveDirection direction) {
        switch (direction) {
            case LEFT -> this.setMapDirection(mapDirection.previous());
            case RIGHT -> this.setMapDirection(mapDirection.next());
            case FORWARD -> this.move(mapDirection.toUnitVector());
            case BACKWARD -> this.move(mapDirection.toUnitVector().opposite());
        }
    }

    public void move(MoveDirection... directions) {
        for(MoveDirection direction : directions) {
            this.move(direction);
        }
    }

    private void move(Vector2D v) {
        Vector2D newPos = this.position.add(v);
        this.position = newPos.isInBounds(lowerLeft, upperRight) ? newPos : position;
    }

    @Override
    public String toString() {
        return TOSTRING_PATTERN
                .replace("{position}", position.toString())
                .replace("{direction}", mapDirection.toString());
    }

}
