package pl.mzlnk.po.lab5.dto;

import pl.mzlnk.po.lab5.map.IMapElement;
import pl.mzlnk.po.lab5.map.IWorldMap;

public abstract class AbstractMapElement implements IMapElement {

    protected static final Vector2D INIT_POSITION = new Vector2D(0, 0);

    protected Vector2D position;
    protected IWorldMap map;

    public AbstractMapElement(IWorldMap map) {
        this(map, INIT_POSITION);
    }

    public AbstractMapElement(IWorldMap map, Vector2D position) {
        this.map = map;
        this.position = position;
    }

    @Override
    public Vector2D getPosition() {
        return position;
    }

}
