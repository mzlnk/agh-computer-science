package pl.mzlnk.conferencegenerator.repository.entity;

import pl.mzlnk.conferencegenerator.model.entity.Entity;
import pl.mzlnk.conferencegenerator.model.entity.EntityType;

import java.util.List;
import java.util.Optional;

public interface EntityRepository<E extends Entity> {

    EntityType getEntityType();

    List<E> findAll();
    Optional<E> findById(int id);
    void createOrUpdate(E entity);
    void remove(int id);

}
