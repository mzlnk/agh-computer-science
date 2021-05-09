package pl.mzlnk.conferencegenerator.repository.data;

import pl.mzlnk.conferencegenerator.model.data.DataType;

import java.util.List;

public interface DataRepository<E> {

    DataType getDataType();
    List<E> randomEntries(int amount);
    List<E> findAll();

}
