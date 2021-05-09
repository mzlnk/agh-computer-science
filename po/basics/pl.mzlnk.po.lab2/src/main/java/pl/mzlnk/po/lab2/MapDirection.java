package pl.mzlnk.po.lab2;

public enum MapDirection {

    NORTH("Północ", new Vector2D(0, 1)),
    EAST("Wschód", new Vector2D(1, 0)),
    WEST("Zachód", new Vector2D(-1, 0)),
    SOUTH("Południe", new Vector2D(0, -1));

    private String name;
    private Vector2D unitVector;

    private MapDirection(String name, Vector2D unitVector) {
        this.name = name;
        this.unitVector = unitVector;
    }

    public MapDirection next() {
        return switch (this) {
            case EAST -> SOUTH;
            case SOUTH -> WEST;
            case WEST -> NORTH;
            case NORTH -> EAST;
        };
    }

    public MapDirection previous() {
        return switch (this) {
            case EAST -> NORTH;
            case SOUTH -> EAST;
            case WEST -> SOUTH;
            case NORTH -> WEST;
        };
    }

    public Vector2D toUnitVector() {
        return unitVector;
    }

    @Override
    public String toString() {
        return name;
    }

}
