package pl.mzlnk.po.lab5.enums;

import pl.mzlnk.po.lab5.dto.Vector2D;

public enum MapDirection {

    NORTH("N", new Vector2D(0, 1)),
    EAST("E", new Vector2D(1, 0)),
    WEST("W", new Vector2D(-1, 0)),
    SOUTH("S", new Vector2D(0, -1));

    private String shortcut;
    private Vector2D unitVector;

    private MapDirection(String shortcut, Vector2D unitVector) {
        this.shortcut = shortcut;
        this.unitVector = unitVector;
    }

    public String getShortcut() {
        return shortcut;
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
        return shortcut;
    }

}
