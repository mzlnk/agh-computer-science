package pl.mzlnk.po.lab2;

public class Vector2D {

    private static final String PATTERN = "({x},{y})";

    public final int x;
    public final int y;

    public Vector2D(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public boolean precedes(Vector2D other) {
        return this.x <= other.x && this.y <= other.y;
    }

    public boolean follows(Vector2D other) {
        return this.x >= other.x && this.y >= other.y;
    }

    public Vector2D upperRight(Vector2D other) {
        return new Vector2D(Math.max(this.x, other.x), Math.max(this.y, other.y));
    }

    public Vector2D lowerLeft(Vector2D other) {
        return new Vector2D(Math.min(this.x, other.x), Math.min(this.y, other.y));
    }

    public Vector2D add(Vector2D other) {
        return new Vector2D(this.x + other.x, this.y + other.y);
    }

    public Vector2D subtract(Vector2D other) {
        return new Vector2D(this.x - other.x, this.y - other.y);
    }

    public Vector2D opposite() {
        return new Vector2D(-this.x, -this.y);
    }

    @Override
    public boolean equals(Object other) {
        if(!(other instanceof Vector2D)) {
            return false;
        }
        Vector2D vector = (Vector2D) other;
        return this.x == vector.x && this.y == vector.y;
    }

    @Override
    public String toString() {
        return PATTERN
                .replace("{x}", String.valueOf(x))
                .replace("{y}", String.valueOf(y));
    }

}
