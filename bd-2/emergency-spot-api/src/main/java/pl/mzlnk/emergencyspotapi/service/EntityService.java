package pl.mzlnk.emergencyspotapi.service;

import pl.mzlnk.emergencyspotapi.model.params.EntityParams;

import java.util.List;
import java.util.Optional;

/**
 * Represents an generalized API for service which mediates between controller and repository layers
 * @param <R> DTO class representing basic information about given object
 * @param <D> DTO class representing detailed information about given object
 * @param <N> DTO class representing information about object to be created
 * @param <E> Entity class representing given object
 */
public interface EntityService<R, D, N, E> {

    /**
     * Obtain list of object based on given parameters
     * @param params search parameters
     * @return list of objects
     */
    List<R> findAll(EntityParams<E> params);

    /**
     * Obtain details about object with given ID
     * @param id object's unique ID
     * @return object details or null if object with given ID does not exist
     */
    Optional<D> findOne(Long id);

    /**
     * Create new object
     * @param entity DTO instance representing information about creating object
     * @return details of created object
     */
    D create(N entity);

    /**
     * Delete existing object
     * @param id object's unique ID
     */
    void deleteById(Long id);

}
