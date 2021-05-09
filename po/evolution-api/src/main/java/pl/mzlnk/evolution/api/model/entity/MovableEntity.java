package pl.mzlnk.evolution.api.model.entity;

import pl.mzlnk.evolution.api.model.world.enums.WorldDirectionEnum;

public interface MovableEntity extends LivingEntity {

    WorldDirectionEnum getMapDirection();
    void move();
    void rotate();

}
