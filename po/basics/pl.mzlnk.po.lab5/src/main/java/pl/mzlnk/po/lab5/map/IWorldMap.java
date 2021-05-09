package pl.mzlnk.po.lab5.map;

import pl.mzlnk.po.lab5.dto.Vector2D;
import pl.mzlnk.po.lab5.enums.MoveDirection;

import java.util.List;
import java.util.Optional;

public interface IWorldMap {

    boolean canMoveTo(Vector2D position);
    boolean place(IMapElement element);
    boolean remove(IMapElement element);
    void run(MoveDirection[] directions);
    boolean isOccupied(Vector2D position);
    Optional<IMapElement> objectAt(Vector2D position);
    List<IMapElement> findAllMapElements();

}
