package pl.mzlnk.po.lab4;

import java.util.Collection;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public enum MoveDirection {

    FORWARD("f", "forward"),
    BACKWARD("b", "backward"),
    RIGHT("r", "right"),
    LEFT("l", "left");

    private String[] shortcuts;

    private MoveDirection(String... shortcuts) {
        this.shortcuts = shortcuts;
    }

    public static Optional<MoveDirection> findByShortcut(String shortcut) {
        return Stream.of(MoveDirection.values())
                .map(d -> Stream.of(d.shortcuts)
                        .map(s -> new Pair<>(d, s))
                        .collect(Collectors.toList()))
                .flatMap(Collection::stream)
                .filter(p -> p.getValue().equals(shortcut))
                .map(Pair::getKey)
                .findFirst();

    }

}
