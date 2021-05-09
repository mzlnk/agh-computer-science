package pl.mzlnk.conferencegenerator.generator.entity;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import pl.mzlnk.conferencegenerator.model.entity.EntityType;
import pl.mzlnk.conferencegenerator.repository.data.DataRepositories;
import pl.mzlnk.conferencegenerator.repository.entity.EntityRepositories;

@Getter
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
abstract class BaseEntityGenerator implements EntityGenerator {

    @EqualsAndHashCode.Include
    private EntityType entityType;

    protected EntityRepositories entityRepositories;
    protected DataRepositories dataRepositories;

    protected int id = 1;

    BaseEntityGenerator(EntityType entityType, EntityRepositories entityRepositories, DataRepositories dataRepositories) {
        this.entityType = entityType;
        this.entityRepositories = entityRepositories;
        this.dataRepositories = dataRepositories;
    }

}
