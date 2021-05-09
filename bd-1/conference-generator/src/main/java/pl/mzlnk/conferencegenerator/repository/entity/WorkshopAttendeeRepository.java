package pl.mzlnk.conferencegenerator.repository.entity;

import pl.mzlnk.conferencegenerator.model.entity.EntityType;
import pl.mzlnk.conferencegenerator.model.entity.attendee.WorkshopAttendee;

class WorkshopAttendeeRepository extends BaseEntityRepository<WorkshopAttendee> {

    WorkshopAttendeeRepository() {
        super(EntityType.WORKSHOP_ATTENDEE);
    }

}
