package pl.mzlnk.po.lab4;

import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class OptionParser {

    public static MoveDirection[] parse(String[] args) {
        return Stream.of(args)
                .map(MoveDirection::findByShortcut)
                .filter(Optional::isPresent)
                .map(Optional::get)
                .toArray(MoveDirection[]::new);
    }

}
