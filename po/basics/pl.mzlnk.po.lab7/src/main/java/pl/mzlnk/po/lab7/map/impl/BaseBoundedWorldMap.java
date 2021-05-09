package pl.mzlnk.po.lab7.map.impl;

import pl.mzlnk.po.lab7.map.element.Vector2D;
import pl.mzlnk.po.lab7.map.element.MapElement;

public abstract class BaseBoundedWorldMap extends BaseWorldMap {

    public BaseBoundedWorldMap() {
        super();
    }

    protected abstract boolean isInBounds(Vector2D position);

    @Override
    public boolean place(MapElement element) {
        if (!isInBounds(element.getPosition())) {
            throw new IllegalArgumentException("Cannot place element - given position out of map bounds");
        }
        return super.place(element);
    }

    @Override
    public boolean canMoveTo(Vector2D position) {
        return super.canMoveTo(position) && isInBounds(position);
    }


}
