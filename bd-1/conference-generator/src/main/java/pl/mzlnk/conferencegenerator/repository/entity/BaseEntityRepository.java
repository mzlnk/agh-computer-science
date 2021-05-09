package pl.mzlnk.conferencegenerator.repository.entity;

import lombok.Getter;
import pl.mzlnk.conferencegenerator.model.entity.Entity;
import pl.mzlnk.conferencegenerator.model.entity.EntityType;

import java.util.*;

abstract class BaseEntityRepository<E extends Entity> implements EntityRepository<E> {

    @Getter
    private EntityType entityType;

    private Map<Integer, E> entities = new HashMap<>();

    BaseEntityRepository(EntityType entityType) {
        this.entityType = entityType;
    }

    @Override
    public List<E> findAll() {
        return new ArrayList<>(entities.values());
    }

    @Override
    public Optional<E> findById(int id) {
        return Optional.ofNullable(entities.get(id));
    }

    @Override
    public void createOrUpdate(E entity) {
        entities.put(entity.getId(), entity);
    }

    @Override
    public void remove(int id) {
        entities.remove(id);
    }

}
