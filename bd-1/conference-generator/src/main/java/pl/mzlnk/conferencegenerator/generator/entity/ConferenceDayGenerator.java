package pl.mzlnk.conferencegenerator.generator.entity;

import pl.mzlnk.conferencegenerator.model.entity.EntityType;
import pl.mzlnk.conferencegenerator.model.entity.conference.Conference;
import pl.mzlnk.conferencegenerator.model.entity.conference.ConferenceDay;
import pl.mzlnk.conferencegenerator.repository.data.DataRepositories;
import pl.mzlnk.conferencegenerator.repository.entity.EntityRepositories;

import java.util.Calendar;

import static pl.mzlnk.conferencegenerator.utils.RandomUtil.r;

class ConferenceDayGenerator extends BaseEntityGenerator {

    ConferenceDayGenerator(EntityRepositories entityRepositories, DataRepositories dataRepositories) {
        super(EntityType.CONFERENCE_DAY, entityRepositories, dataRepositories);
    }

    @Override
    public void generate() {
        var conferenceRepository = entityRepositories.<Conference>getRepository(EntityType.CONFERENCE);
        var conferenceDayRepository = entityRepositories.<ConferenceDay>getRepository(EntityType.CONFERENCE_DAY);

        conferenceRepository
                .findAll()
                .stream()
                .forEach(conference -> {
                    int days = (r.nextBoolean() ? 2 : 3);

                    Calendar conferenceDayDate = Calendar.getInstance();
                    conferenceDayDate.set(
                            conference.getDate().get(Calendar.YEAR),
                            conference.getDate().get(Calendar.MONTH),
                            r.nextInt(24) + 1,
                            8,
                            0);

                    for (int i = 0; i < days; i++) {
                        conferenceDayRepository.createOrUpdate(new ConferenceDay(id++, conference.getConferenceId(), conferenceDayDate));
                        conferenceDayDate.add(Calendar.DAY_OF_MONTH, 1);
                    }
                });

        System.out.println("generated " + conferenceDayRepository.findAll().size() + " conference day entities");
    }

}
