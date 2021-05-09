package pl.mzlnk.po.lab5;

import pl.mzlnk.po.lab5.dto.Animal;
import pl.mzlnk.po.lab5.dto.Vector2D;
import pl.mzlnk.po.lab5.enums.MoveDirection;
import pl.mzlnk.po.lab5.map.IWorldMap;
import pl.mzlnk.po.lab5.map.impl.RectangularMap;
import pl.mzlnk.po.lab5.utils.OptionParser;

public class World {

    public static void main(String[] args) {
        MoveDirection[] directions = OptionParser.parse(args);

        IWorldMap map = new RectangularMap(10, 5);
        map.place(new Animal(map));
        map.place(new Animal(map, new Vector2D(3, 4)));

        map.run(directions);
    }

}
