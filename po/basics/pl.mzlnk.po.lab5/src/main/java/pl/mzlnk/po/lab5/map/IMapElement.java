package pl.mzlnk.po.lab5.map;

import pl.mzlnk.po.lab5.dto.Vector2D;
import pl.mzlnk.po.lab5.enums.MoveDirection;

public interface IMapElement {

    Vector2D getPosition();
    void move(MoveDirection moveDirection);

}
