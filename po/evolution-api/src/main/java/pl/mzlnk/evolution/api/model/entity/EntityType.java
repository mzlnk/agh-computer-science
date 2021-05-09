package pl.mzlnk.evolution.api.model.entity;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;
import pl.mzlnk.evolution.api.model.entity.impl.Animal;
import pl.mzlnk.evolution.api.model.entity.impl.Grass;

@Getter
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public enum EntityType {

    ANIMAL(Animal.class, "A", 1),
    GRASS(Grass.class, "G", 0);

    private Class<? extends Entity> clazz;
    private String shortcut;
    private int priority;

}
