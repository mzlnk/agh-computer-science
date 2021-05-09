package pl.mzlnk.po.lab7.map;

import pl.mzlnk.po.lab7.map.element.Vector2D;

public interface PositionChangedObserver {

    void positionChanged(Vector2D oldPosition, Vector2D newPosition);

}
