package pl.mzlnk.conferencegenerator.repository.data;

import pl.mzlnk.conferencegenerator.model.data.DataType;
import pl.mzlnk.conferencegenerator.model.data.Phone;
import pl.mzlnk.conferencegenerator.service.FileService;

import static pl.mzlnk.conferencegenerator.utils.RandomUtil.r;

class PhoneRepository extends BaseDataRepository<Phone> {

    PhoneRepository(FileService fileService) {
        super(DataType.PHONE, fileService);
    }

    @Override
    protected Phone randomEntry() {
        String direction = "0" + (r.nextInt(90) + 10);
        return new Phone(direction + (r.nextInt(900000000) + 100000000));
    }

    @Override
    protected void loadData(FileService fileService) {

    }

}
