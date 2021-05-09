package pl.mzlnk.conferencegenerator.generator.entity;

import pl.mzlnk.conferencegenerator.model.data.Address;
import pl.mzlnk.conferencegenerator.model.data.DataType;
import pl.mzlnk.conferencegenerator.model.data.Person;
import pl.mzlnk.conferencegenerator.model.entity.EntityType;
import pl.mzlnk.conferencegenerator.model.entity.attendee.Company;
import pl.mzlnk.conferencegenerator.model.entity.order.Buyer;
import pl.mzlnk.conferencegenerator.repository.data.DataRepositories;
import pl.mzlnk.conferencegenerator.repository.entity.EntityRepositories;

import java.util.List;

import static pl.mzlnk.conferencegenerator.utils.RandomUtil.r;

class BuyerGenerator extends BaseEntityGenerator {

    BuyerGenerator(EntityRepositories entityRepositories, DataRepositories dataRepositories) {
        super(EntityType.BUYER, entityRepositories, dataRepositories);
    }

    @Override
    public void generate() {
        var buyerRepository = entityRepositories.<Buyer>getRepository(EntityType.BUYER);
        var companyRepository = entityRepositories.<Company>getRepository(EntityType.COMPANY);

        List<Company> companies = companyRepository.findAll();

        var personRepository = dataRepositories.<Person>getRepository(DataType.PERSON);
        var addressRepository = dataRepositories.<Address>getRepository(DataType.ADDRESS);

        for (int i = 0; i < 50; i++) {
            int buyerId = id++;
            int companyId = (r.nextBoolean() ? companies.get(r.nextInt(companies.size())).getCompanyId() : -1);

            Person person = personRepository.randomEntries(1).get(0);
            String firstName = person.getFirstName();
            String lastName = person.getLastName();

            String address = addressRepository.randomEntries(1).get(0).getAddress();

            buyerRepository.createOrUpdate(new Buyer(buyerId, companyId, firstName, lastName, address));
        }

        System.out.println("generated: " + buyerRepository.findAll().size() + " buyer entities");
    }

}
