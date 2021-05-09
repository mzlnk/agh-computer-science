package pl.mzlnk.conferencegenerator.repository.data;

import lombok.Getter;
import pl.mzlnk.conferencegenerator.model.data.DataType;
import pl.mzlnk.conferencegenerator.service.FileService;

import java.util.ArrayList;
import java.util.List;

abstract class BaseDataRepository<E> implements DataRepository<E> {

    @Getter
    private DataType dataType;

    BaseDataRepository(DataType dataType, FileService fileService) {
        this.dataType = dataType;
        loadData(fileService);
    }

    protected abstract E randomEntry();
    protected abstract void loadData(FileService fileService);

    @Override
    public List<E> randomEntries(int amount) {
        List<E> entries = new ArrayList<>();

        for(int i = 0; i < amount; i++) {
            entries.add(randomEntry());
        }

        return entries;
    }

    @Override
    public List<E> findAll() {
        return new ArrayList<>();
    }

}
