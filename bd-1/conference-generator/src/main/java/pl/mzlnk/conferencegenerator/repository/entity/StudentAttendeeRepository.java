package pl.mzlnk.conferencegenerator.repository.entity;

import pl.mzlnk.conferencegenerator.model.entity.EntityType;
import pl.mzlnk.conferencegenerator.model.entity.attendee.StudentAttendee;

class StudentAttendeeRepository extends BaseEntityRepository<StudentAttendee> {

    StudentAttendeeRepository() {
        super(EntityType.STUDENT_ATTENDEE);
    }

}
