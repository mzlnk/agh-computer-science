package pl.mzlnk.po.lab5.dto;

import pl.mzlnk.po.lab5.enums.MoveDirection;
import pl.mzlnk.po.lab5.map.IWorldMap;

public class Grass extends AbstractMapElement {

    public Grass(IWorldMap map, Vector2D position) {
        super(map, position);
    }

    @Override
    public void move(MoveDirection moveDirection) {
        // empty method body - element cannot move
    }

    @Override
    public String toString() {
        return "s";
    }

}
