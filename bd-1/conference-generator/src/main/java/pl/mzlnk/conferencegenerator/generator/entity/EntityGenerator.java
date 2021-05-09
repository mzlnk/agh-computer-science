package pl.mzlnk.conferencegenerator.generator.entity;

import pl.mzlnk.conferencegenerator.model.entity.EntityType;

public interface EntityGenerator {

    EntityType getEntityType();
    void generate();

}
