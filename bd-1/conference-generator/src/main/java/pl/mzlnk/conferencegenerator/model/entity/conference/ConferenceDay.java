package pl.mzlnk.conferencegenerator.model.entity.conference;

import lombok.Builder;
import lombok.Getter;
import pl.mzlnk.conferencegenerator.model.entity.BaseEntity;
import pl.mzlnk.conferencegenerator.model.entity.EntityType;
import pl.mzlnk.conferencegenerator.generator.sql.utils.annotation.*;

import java.util.Calendar;

@Getter
@TableOrder(1)
@Table(name = "conference_day")
public class ConferenceDay extends BaseEntity {

    @Builder
    public ConferenceDay(int conferenceDayId, int conferenceId, Calendar date) {
        super(EntityType.CONFERENCE_DAY, conferenceDayId);

        this.conferenceDayId = conferenceDayId;
        this.conferenceId = conferenceId;
        this.date = date;
    }

    @PrimaryKey
    @AutoIncrement
    @Column(name = "conference_day_id")
    private int conferenceDayId;

    @ForeignKey(entity = Conference.class, column = "conference_id")
    @Column(name = "conference_id")
    private int conferenceId;

    @Column(name = "date")
    private Calendar date;

}
