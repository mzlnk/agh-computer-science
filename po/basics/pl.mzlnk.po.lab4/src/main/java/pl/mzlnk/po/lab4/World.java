package pl.mzlnk.po.lab4;

public class World {

    public static void main(String[] args) {
        MoveDirection[] directions = OptionParser.parse(args);

        IWorldMap map = new RectangularMap(10, 5);
        map.place(new Animal(map));
        map.place(new Animal(map, new Vector2D(3, 4)));

        map.run(directions);
    }

}
