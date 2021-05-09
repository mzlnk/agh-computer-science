package pl.mzlnk.conferencegenerator.model.entity.conference;

import lombok.Getter;
import pl.mzlnk.conferencegenerator.generator.sql.utils.annotation.*;
import pl.mzlnk.conferencegenerator.model.entity.BaseEntity;
import pl.mzlnk.conferencegenerator.model.entity.EntityType;

import java.util.Calendar;

@Getter
@TableOrder(0)
@Table(name = "conference")
public class Conference extends BaseEntity {

    public Conference(int conferenceId, String name, String description, int studentDiscount, Calendar date) {
        super(EntityType.CONFERENCE, conferenceId);

        this.conferenceId = conferenceId;
        this.name = name;
        this.description = description;
        this.studentDiscount = studentDiscount;
        this.date = date;
    }

    @PrimaryKey
    @AutoIncrement
    @Column(name = "conference_id")
    private int conferenceId;

    @Column(name = "name")
    private String name;

    @Column(name = "description")
    private String description;

    @Column(name = "student_discount")
    private int studentDiscount;

    private Calendar date;

}
