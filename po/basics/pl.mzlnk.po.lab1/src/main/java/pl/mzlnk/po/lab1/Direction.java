package pl.mzlnk.po.lab1;

import java.util.Optional;
import java.util.stream.Stream;

public enum Direction {

    FORWARD("f", () -> System.out.println("Moving forward...")),
    BACKWARD("b", () -> System.out.println("Moving backward...")),
    LEFT("l", () -> System.out.println("Moving left...")),
    RIGHT("r", () -> System.out.println("Moving right..."));

    private String shortcut;
    private Runnable runnable;

    public String getShortcut() {
        return shortcut;
    }

    public void move() {
        this.runnable.run();
    }

    private Direction(String shortcut, Runnable runnable) {
        this.shortcut = shortcut;
        this.runnable = runnable;
    }

    public static Optional<Direction> getByShortcut(String shortcut) {
        return Stream.of(Direction.values())
                .filter(direction -> direction.shortcut.equals(shortcut))
                .findAny();
    }

}
