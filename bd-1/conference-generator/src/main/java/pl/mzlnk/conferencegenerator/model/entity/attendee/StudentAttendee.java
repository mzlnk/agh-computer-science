package pl.mzlnk.conferencegenerator.model.entity.attendee;

import lombok.Builder;
import lombok.Getter;
import pl.mzlnk.conferencegenerator.generator.sql.utils.annotation.*;
import pl.mzlnk.conferencegenerator.model.entity.BaseEntity;
import pl.mzlnk.conferencegenerator.model.entity.EntityType;

@Getter
@TableOrder(2)
@Table(name = "student_attendee")
public class StudentAttendee extends BaseEntity {

    @Builder
    public StudentAttendee(int attendeeId, int studentCardId) {
        super(EntityType.STUDENT_ATTENDEE, attendeeId);

        this.attendeeId = attendeeId;
        this.studentCardId = studentCardId;
    }

    @PrimaryKey
    @ForeignKey(entity = Attendee.class, column = "attendee_id")
    @Column(name = "attendee_id")
    private int attendeeId;

    @Column(name = "student_card_id")
    private int studentCardId;

}
