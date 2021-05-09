package pl.mzlnk.conferencegenerator.model.entity;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NonNull;

@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public abstract class BaseEntity implements Entity {

    @Getter
    private EntityType entityType;

    @Getter
    @EqualsAndHashCode.Include
    private int id;

    public BaseEntity(@NonNull EntityType entityType, int id) {
        this.entityType = entityType;
        this.id = id;
    }

}
