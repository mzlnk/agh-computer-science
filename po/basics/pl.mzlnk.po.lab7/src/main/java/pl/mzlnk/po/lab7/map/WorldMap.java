package pl.mzlnk.po.lab7.map;

import pl.mzlnk.po.lab7.map.element.MapElement;
import pl.mzlnk.po.lab7.map.element.Vector2D;
import pl.mzlnk.po.lab7.enums.MoveDirection;

import java.util.List;
import java.util.Optional;

public interface WorldMap {

    boolean canMoveTo(Vector2D position);
    boolean place(MapElement element);
    boolean remove(MapElement element);
    void run(MoveDirection[] directions);
    boolean isOccupied(Vector2D position);
    Optional<MapElement> objectAt(Vector2D position);
    List<MapElement> findAllMapElements();

}
