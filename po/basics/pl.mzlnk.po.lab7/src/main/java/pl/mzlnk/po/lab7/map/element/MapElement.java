package pl.mzlnk.po.lab7.map.element;

import pl.mzlnk.po.lab7.enums.MoveDirection;

public interface MapElement {

    Vector2D getPosition();
    void move(MoveDirection moveDirection);

}
