package pl.mzlnk.conferencegenerator.repository.entity;

import pl.mzlnk.conferencegenerator.model.entity.EntityType;
import pl.mzlnk.conferencegenerator.model.entity.attendee.Attendee;

class AttendeeRepository extends BaseEntityRepository<Attendee> {

    AttendeeRepository() {
        super(EntityType.ATTENDEE);
    }

}
