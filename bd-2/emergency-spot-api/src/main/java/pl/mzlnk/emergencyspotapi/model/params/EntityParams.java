package pl.mzlnk.emergencyspotapi.model.params;

import org.springframework.data.domain.Example;

/**
 * Interfaces representing entity search params used to obtain proper result from database
 * @param <E> entity type
 */
public interface EntityParams<E> {

    Example<E> toExample();

}
