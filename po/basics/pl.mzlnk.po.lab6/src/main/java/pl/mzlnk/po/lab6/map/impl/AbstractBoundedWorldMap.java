package pl.mzlnk.po.lab6.map.impl;

import pl.mzlnk.po.lab6.dto.Vector2D;
import pl.mzlnk.po.lab6.map.IMapElement;

public abstract class AbstractBoundedWorldMap extends AbstractWorldMap {

    public AbstractBoundedWorldMap() {
        super();
    }

    protected abstract boolean isInBounds(Vector2D position);

    @Override
    public boolean place(IMapElement element) {
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
