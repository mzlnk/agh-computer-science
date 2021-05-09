package pl.mzlnk.conferencegenerator.generator.entity;

import pl.mzlnk.conferencegenerator.model.entity.EntityType;
import pl.mzlnk.conferencegenerator.model.entity.attendee.Attendee;
import pl.mzlnk.conferencegenerator.model.entity.attendee.StudentAttendee;
import pl.mzlnk.conferencegenerator.repository.data.DataRepositories;
import pl.mzlnk.conferencegenerator.repository.entity.EntityRepositories;

import static pl.mzlnk.conferencegenerator.utils.RandomUtil.r;

class StudentAttendeeGenerator extends BaseEntityGenerator {

    StudentAttendeeGenerator(EntityRepositories entityRepositories, DataRepositories dataRepositories) {
        super(EntityType.STUDENT_ATTENDEE, entityRepositories, dataRepositories);
    }

    @Override
    public void generate() {
        var studentRepository = entityRepositories.<StudentAttendee>getRepository(EntityType.STUDENT_ATTENDEE);
        var attendeeRepository = entityRepositories.<Attendee>getRepository(EntityType.ATTENDEE);

        final int attendeesSize = attendeeRepository.findAll().size();

        for(int i = 0; i < attendeesSize; i++) {
            if(r.nextDouble() < 0.3) {
                int attendeeId = (i + 1);
                int studentCardId = r.nextInt(500000) + 100000;
                studentRepository.createOrUpdate(new StudentAttendee(attendeeId, studentCardId));
            }
        }

        System.out.println("generated " + studentRepository.findAll().size() + " student attendee entities");
    }

}
