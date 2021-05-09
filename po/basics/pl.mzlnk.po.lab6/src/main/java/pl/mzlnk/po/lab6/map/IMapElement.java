package pl.mzlnk.po.lab6.map;

import pl.mzlnk.po.lab6.dto.Vector2D;
import pl.mzlnk.po.lab6.enums.MoveDirection;

public interface IMapElement {

    Vector2D getPosition();
    void move(MoveDirection moveDirection);

}
