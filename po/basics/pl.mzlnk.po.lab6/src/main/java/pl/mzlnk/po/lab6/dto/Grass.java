package pl.mzlnk.po.lab6.dto;

import pl.mzlnk.po.lab6.map.IWorldMap;

public class Grass extends AbstractMapElement {

    public Grass(IWorldMap map, Vector2D position) {
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
