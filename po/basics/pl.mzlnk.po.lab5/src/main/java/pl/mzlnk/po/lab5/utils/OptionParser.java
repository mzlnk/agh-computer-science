package pl.mzlnk.po.lab5.utils;

import pl.mzlnk.po.lab5.enums.MoveDirection;

import java.util.Optional;
import java.util.stream.Stream;

public class OptionParser {

    public static MoveDirection[] parse(String[] args) {
        return Stream.of(args)
                .map(MoveDirection::findByShortcut)
                .filter(Optional::isPresent)
                .map(Optional::get).toArray(MoveDirection[]::new);
    }

}
