package pl.mzlnk.po.lab7.map.element;

import pl.mzlnk.po.lab7.map.WorldMap;

public class Grass extends BaseMapElement {

    public Grass(WorldMap map, Vector2D position) {
        super(map, position);
    }

    @Override
    public Vector2D getPosition() {
        return position;
    }

    @Override
    public String toString() {
        return "s";
    }

}
