package pl.mzlnk.conferencegenerator.generator.entity;

import pl.mzlnk.conferencegenerator.model.data.ConferenceDetails;
import pl.mzlnk.conferencegenerator.model.data.DataType;
import pl.mzlnk.conferencegenerator.model.entity.EntityType;
import pl.mzlnk.conferencegenerator.model.entity.conference.Conference;
import pl.mzlnk.conferencegenerator.repository.data.DataRepositories;
import pl.mzlnk.conferencegenerator.repository.entity.EntityRepositories;
import pl.mzlnk.conferencegenerator.utils.CalendarUtil;

import java.util.Calendar;

import static pl.mzlnk.conferencegenerator.utils.RandomUtil.r;

class ConferenceGenerator extends BaseEntityGenerator {

    ConferenceGenerator(EntityRepositories entityRepositories, DataRepositories dataRepositories) {
        super(EntityType.CONFERENCE, entityRepositories, dataRepositories);
    }

    @Override
    public void generate() {
        var conferenceRepository = entityRepositories.<Conference>getRepository(EntityType.CONFERENCE);
        var conferencesNamesRepository = dataRepositories.<ConferenceDetails>getRepository(DataType.CONFERENCE_DETAILS);

        Calendar start = CalendarUtil.fromString("2017-01-01 00:00:00");
        Calendar end = CalendarUtil.fromString("2020-12-31 23:59:59");

        for(Calendar cal = (Calendar) start.clone(); cal.before(end); cal.add(Calendar.MONTH, 1)) {
            int conferences = r.nextInt(3) + 1;
            conferencesNamesRepository.randomEntries(conferences)
                    .stream()
                    .map(details -> new Conference(id++, details.getName(), details.getDescription(), r.nextInt(100), (Calendar) cal.clone()))
                    .forEach(conferenceRepository::createOrUpdate);
        }

        System.out.println("generated: " + conferenceRepository.findAll().size() + " conference entities");
    }

}
