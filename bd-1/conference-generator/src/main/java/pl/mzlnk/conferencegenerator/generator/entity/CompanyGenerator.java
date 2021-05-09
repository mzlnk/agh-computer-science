package pl.mzlnk.conferencegenerator.generator.entity;

import pl.mzlnk.conferencegenerator.model.data.Address;
import pl.mzlnk.conferencegenerator.model.data.CompanyName;
import pl.mzlnk.conferencegenerator.model.data.DataType;
import pl.mzlnk.conferencegenerator.model.data.Phone;
import pl.mzlnk.conferencegenerator.model.entity.EntityType;
import pl.mzlnk.conferencegenerator.model.entity.attendee.Company;
import pl.mzlnk.conferencegenerator.repository.data.DataRepositories;
import pl.mzlnk.conferencegenerator.repository.entity.EntityRepositories;

class CompanyGenerator extends BaseEntityGenerator {

    CompanyGenerator(EntityRepositories entityRepositories, DataRepositories dataRepositories) {
        super(EntityType.COMPANY, entityRepositories, dataRepositories);
    }

    @Override
    public void generate() {
        var companyRepository = entityRepositories.<Company>getRepository(EntityType.COMPANY);

        var companyNameRepository = dataRepositories.<CompanyName>getRepository(DataType.COMPANY_NAME);
        var addressRepository = dataRepositories.<Address>getRepository(DataType.ADDRESS);
        var phoneRepository = dataRepositories.<Phone>getRepository(DataType.PHONE);

        companyNameRepository
                .findAll()
                .stream()
                .map(companyName -> new Company(id++,
                        companyName.getCompanyName(),
                        addressRepository.randomEntries(1).get(0).getAddress(),
                        phoneRepository.randomEntries(1).get(0).getPhone()))
                .forEach(companyRepository::createOrUpdate);

        System.out.println("generated: " + companyRepository.findAll().size() + " company entities");
    }

}
