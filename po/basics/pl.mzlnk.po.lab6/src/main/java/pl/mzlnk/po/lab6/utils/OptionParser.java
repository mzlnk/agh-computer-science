package pl.mzlnk.po.lab6.utils;

import pl.mzlnk.po.lab6.enums.MoveDirection;

import java.util.Optional;
import java.util.stream.Stream;

public class OptionParser {

    public static MoveDirection[] parse(String[] args) {
        return Stream.of(args)
                .map(arg -> MoveDirection.findByShortcut(arg).orElseThrow(() -> new IllegalArgumentException(arg + " is not legal move specification")))
                .toArray(MoveDirection[]::new);
    }

}
