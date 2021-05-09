package pl.mzlnk.evolution.api.model.world.enums;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;
import pl.mzlnk.evolution.api.model.location.Location;

import java.util.Optional;
import java.util.stream.Stream;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public enum WorldDirectionEnum {

    NORTH(new Location(0, 1), 0),
    SOUTH(new Location(0, -1), 180),
    EAST(new Location(1, 0), 90),
    WEST(new Location(-1, 0), 270),
    EAST_SOUTH(new Location(-1, -1), 135),
    WEST_SOUTH(new Location(1, -1), 225),
    EAST_NORTH(new Location(-1, 1), 45),
    WEST_NORTH(new Location(1, 1), 315);

    public static final int UNIT_DEGREE_CHANGE = 45;

    @Getter
    private Location unitVector;

    @Getter
    private int angle;

    public WorldDirectionEnum rotate(int degrees) {
        int newAngle = (this.angle + degrees) % 360;
        return getByAngle(newAngle).orElseThrow(NoSuchMapDirectionException::new);
    }

    public static WorldDirectionEnum randomMapDirection() {
        return Stream.of(values())
                .findAny()
                .orElse(null);
    }

    private static Optional<WorldDirectionEnum> getByAngle(int angle) {
        return Stream.of(values())
                .filter(e -> e.angle == angle)
                .findFirst();
    }

    private static class NoSuchMapDirectionException extends RuntimeException {

        public NoSuchMapDirectionException() {
            super("Map direction with given angle not found");
        }

    }

}
