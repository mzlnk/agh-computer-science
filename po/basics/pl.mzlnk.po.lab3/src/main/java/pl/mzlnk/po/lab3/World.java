package pl.mzlnk.po.lab3;

import java.util.stream.Stream;

public class World {

    public static void main(String[] args) {
        Animal animal = new Animal();
        System.out.println(animal.toString());

        String[] moves = new String[]{"f", "b", "backward", "left", "right", "r", "f", "f", "f", "b"};

        Stream.of(OptionsParser.parse(moves))
                .forEach(m -> {
                    animal.move(m);
                    System.out.println(animal.toString());
                });

    }

}
