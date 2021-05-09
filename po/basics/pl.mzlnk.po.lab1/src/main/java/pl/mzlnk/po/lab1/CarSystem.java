package pl.mzlnk.po.lab1;

import java.util.stream.Stream;

public class CarSystem {

    public static void main(String[] args) {
        System.out.println("Car system started.");

        Stream.of(args)
                .map(Direction::getByShortcut)
                .forEach(direction -> {
                    direction.ifPresentOrElse(Direction::move, () -> System.out.println("Invalid arg!"));
                });

        System.out.println("Car system stopped.");
    }

}
