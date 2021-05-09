package pl.mzlnk.po.lab3;

import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class OptionsParser {

    public static MoveDirection[] parse(String[] args) {
        return Stream.of(args)
                .map(MoveDirection::findByShortcut)
                .map(Optional::get)
                .collect(Collectors.toList())
                .toArray(new MoveDirection[0]);
    }

}
