package pl.mzlnk.conferencegenerator.repository.entity;

import pl.mzlnk.conferencegenerator.model.entity.EntityType;
import pl.mzlnk.conferencegenerator.model.entity.conference.ConferenceDay;

class ConferenceDayRepository extends BaseEntityRepository<ConferenceDay> {

    ConferenceDayRepository() {
        super(EntityType.CONFERENCE_DAY);
    }

}
