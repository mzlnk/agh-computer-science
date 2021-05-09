package pl.mzlnk.conferencegenerator.generator.sql.utils.parser;

import pl.mzlnk.conferencegenerator.generator.sql.utils.annotation.TableOrder;
import pl.mzlnk.conferencegenerator.generator.sql.utils.annotation.Table;
import pl.mzlnk.conferencegenerator.model.entity.Entity;
import pl.mzlnk.conferencegenerator.model.entity.EntityType;
import pl.mzlnk.conferencegenerator.repository.entity.EntityRepository;

import java.lang.reflect.Field;
import java.util.Optional;

public class SqlEntityRepositoryParser {

    private EntityType entityType;

    public SqlEntityRepositoryParser(EntityRepository<? extends Entity> repository) {
        this(repository.getEntityType());
    }

    public SqlEntityRepositoryParser(EntityType entityType) {
        this.entityType = entityType;
    }

    public String getTableName() {
        return entityType
                .getClazz()
                .getDeclaredAnnotation(Table.class)
                .name();
    }

    public int getOrder() {
        return Optional.of(entityType.getClazz().getDeclaredAnnotation(TableOrder.class))
                .map(TableOrder::value)
                .orElse(0);
    }

    public Field[] getFields() {
        return entityType
                .getClazz()
                .getDeclaredFields();
    }

}
