package pl.mzlnk.po.lab6;

import pl.mzlnk.po.lab6.dto.Animal;
import pl.mzlnk.po.lab6.dto.Vector2D;
import pl.mzlnk.po.lab6.enums.MoveDirection;
import pl.mzlnk.po.lab6.map.IWorldMap;
import pl.mzlnk.po.lab6.map.impl.GrassFieldMap;
import pl.mzlnk.po.lab6.utils.OptionParser;

public class World {

    public static void main(String[] args) {
        String[] moves = new String[]{"f", "b", "backward", "left", "right", "r", "f", "f", "f", "b"};

        IWorldMap map = new GrassFieldMap(10);
        Animal animal1 = new Animal(map, new Vector2D(20, 20));
        Animal animal2 = new Animal(map, new Vector2D(20, 20));

        Vector2D a = new Vector2D(0, 0);
        Vector2D b = new Vector2D(0, 0);

        System.out.println(a.hashCode());
        System.out.println(b.hashCode());

        try {
            MoveDirection[] directions = OptionParser.parse(moves);
            map.place(animal1);
            map.place(animal2);

            map.run(directions);
        } catch (IllegalArgumentException e) {
            e.printStackTrace();
        }

    }

}
