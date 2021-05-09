package pl.mzlnk.conferencegenerator.generator.entity;

import pl.mzlnk.conferencegenerator.model.data.DataType;
import pl.mzlnk.conferencegenerator.model.data.WorkshopDetails;
import pl.mzlnk.conferencegenerator.model.entity.EntityType;
import pl.mzlnk.conferencegenerator.model.entity.conference.ConferenceDay;
import pl.mzlnk.conferencegenerator.model.entity.conference.Workshop;
import pl.mzlnk.conferencegenerator.repository.data.DataRepositories;
import pl.mzlnk.conferencegenerator.repository.entity.EntityRepositories;

import java.util.Calendar;

import static pl.mzlnk.conferencegenerator.utils.RandomUtil.r;

class WorkshopGenerator extends BaseEntityGenerator {

    WorkshopGenerator(EntityRepositories entityRepositories, DataRepositories dataRepositories) {
        super(EntityType.WORKSHOP, entityRepositories, dataRepositories);
    }

    @Override
    public void generate() {
        var workshopRepository = entityRepositories.<Workshop>getRepository(EntityType.WORKSHOP);
        var conferenceDayRepository = entityRepositories.<ConferenceDay>getRepository(EntityType.CONFERENCE_DAY);

        var workshopDetailsRepository = dataRepositories.<WorkshopDetails>getRepository(DataType.WORKSHOP_DETAILS);

        conferenceDayRepository
                .findAll()
                .forEach(conferenceDay -> {
                    int workshops = r.nextInt(6) + 1;

                    workshopDetailsRepository.randomEntries(workshops)
                            .stream()
                            .map( workshopDetails -> {
                                Calendar dateStart = (Calendar) conferenceDay.getDate().clone();
                                dateStart.set(Calendar.HOUR_OF_DAY, r.nextInt(12) + 8);

                                Calendar dateEnd = (Calendar) dateStart.clone();
                                dateEnd.add(Calendar.HOUR_OF_DAY, r.nextInt(24 - dateStart.get(Calendar.HOUR_OF_DAY)) + 1);

                                return new Workshop(
                                        id++,
                                        conferenceDay.getConferenceDayId(),
                                        workshopDetails.getName(),
                                        workshopDetails.getDescription(),
                                        workshopDetails.getPrice(),
                                        dateStart,
                                        dateEnd,
                                        workshopDetails.getAttendeesLimit()
                                );
                            })
                            .forEach(workshopRepository::createOrUpdate);
                });

        System.out.println("generated " + workshopRepository.findAll().size() + " workshop entities");
    }

}
