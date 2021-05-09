package pl.mzlnk.conferencegenerator.generator.entity;

import pl.mzlnk.conferencegenerator.model.data.DataType;
import pl.mzlnk.conferencegenerator.model.data.Person;
import pl.mzlnk.conferencegenerator.model.entity.EntityType;
import pl.mzlnk.conferencegenerator.model.entity.attendee.Attendee;
import pl.mzlnk.conferencegenerator.model.entity.attendee.Company;
import pl.mzlnk.conferencegenerator.repository.data.DataRepositories;
import pl.mzlnk.conferencegenerator.repository.entity.EntityRepositories;

import java.util.ArrayList;
import java.util.List;

import static pl.mzlnk.conferencegenerator.utils.RandomUtil.r;

class AttendeeGenerator extends BaseEntityGenerator {

    AttendeeGenerator(EntityRepositories entityRepositories, DataRepositories dataRepositories) {
        super(EntityType.ATTENDEE, entityRepositories, dataRepositories);
    }

    @Override
    public void generate() {
        var attendeeRepository = entityRepositories.<Attendee>getRepository(EntityType.ATTENDEE);
        var companyRepository = entityRepositories.<Company>getRepository(EntityType.COMPANY);
        var personRepository = dataRepositories.<Person>getRepository(DataType.PERSON);

        final int companiesSize = companyRepository.findAll().size();
        List<Person> people = personRepository.randomEntries(1500);


        List<String> addedEmails = new ArrayList<>();
        for(int i = 0; i < 1500; i++) {
            int attendeeId = (i + 1);
            String firstName = people.get(i).getFirstName();
            String lastName = people.get(i).getLastName();
            int companyId = (r.nextDouble() > 0.4) ? r.nextInt(companiesSize) + 1 : -1;

            String companyName = companyRepository.findById(companyId).map(Company::getName).orElse(null);
            String email = "";
            do {
                email = (firstName + "." + lastName + r.nextInt(5000) + "@" + (companyName != null ? companyName : "gmail") + ".com").toLowerCase().replace(" ", "");
            } while(addedEmails.contains(email));

            addedEmails.add(email);
            attendeeRepository.createOrUpdate(new Attendee(attendeeId, firstName, lastName, email, companyId));
        }

        System.out.println("generated: " + attendeeRepository.findAll().size() + " attendee entities");
    }

}
