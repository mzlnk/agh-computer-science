package pl.mzlnk.po.lab7.map.element;

import pl.mzlnk.po.lab7.enums.MoveDirection;
import pl.mzlnk.po.lab7.map.WorldMap;

public abstract class BaseMapElement implements MapElement {

    protected static final Vector2D INIT_POSITION = new Vector2D(0, 0);

    protected Vector2D position;
    protected WorldMap map;

    public BaseMapElement(WorldMap map) {
        this(map, INIT_POSITION);
    }

    public BaseMapElement(WorldMap map, Vector2D position) {
        this.map = map;
        this.position = position;
    }

    @Override
    public Vector2D getPosition() {
        return position;
    }

    @Override
    public void move(MoveDirection moveDirection) {

    }

}
