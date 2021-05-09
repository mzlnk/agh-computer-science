package pl.mzlnk.evolution.api.model.location;

import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import pl.mzlnk.evolution.api.model.world.move_strategy.WorldMoveStrategy;

@EqualsAndHashCode
@AllArgsConstructor
public class Location {

    private static final String TOSTRING_PATTERN = "({x}, {y})";

    public final int x;
    public final int y;

    public boolean between(Location lowerLeft, Location upperRight) {
        return this.x >= lowerLeft.x &&
                this.x <= upperRight.x &&
                this.y >= lowerLeft.y &&
                this.y <= upperRight.y;
    }

    public Location add(Location vector) {
        return new Location(this.x + vector.x, this.y + vector.y);
    }

    public Location add(Location vector, WorldMoveStrategy strategy) {
        return strategy.add(this, vector);
    }

    @Override
    public String toString() {
        return TOSTRING_PATTERN
                .replace("{x}", String.valueOf(this.x))
                .replace("{y}", String.valueOf(this.y));
    }

}
