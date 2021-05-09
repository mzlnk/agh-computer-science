package pl.mzlnk.po.lab7.map.impl;

import pl.mzlnk.po.lab7.map.element.Vector2D;
import pl.mzlnk.po.lab7.utils.Pair;

public class RectangularMap extends BaseBoundedWorldMap {

    private final Vector2D lowerLeft;
    private final Vector2D upperRight;

    public RectangularMap(int width, int height) {
        this.lowerLeft = new Vector2D(0, 0);
        this.upperRight = new Vector2D(width - 1, height - 1);
    }

    @Override
    protected boolean isInBounds(Vector2D position) {
        return position.isInBounds(lowerLeft, upperRight);
    }

}
