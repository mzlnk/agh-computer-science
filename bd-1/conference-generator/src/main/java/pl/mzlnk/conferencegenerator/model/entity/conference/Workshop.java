package pl.mzlnk.conferencegenerator.model.entity.conference;

import lombok.Builder;
import lombok.Getter;
import pl.mzlnk.conferencegenerator.model.entity.BaseEntity;
import pl.mzlnk.conferencegenerator.model.entity.EntityType;
import pl.mzlnk.conferencegenerator.generator.sql.utils.annotation.*;

import java.util.Calendar;

@Getter
@TableOrder(2)
@Table(name = "workshop")
public class Workshop extends BaseEntity {

    @Builder
    public Workshop(int workshopId, int conferenceDayId, String name, String description, int price, Calendar dateStart, Calendar dateEnd, int attendeesLimit) {
        super(EntityType.WORKSHOP, workshopId);

        this.workshopId = workshopId;
        this.conferenceDayId = conferenceDayId;
        this.name = name;
        this.description = description;
        this.price = price;
        this.dateStart = dateStart;
        this.dateEnd = dateEnd;
        this.attendeesLimit = attendeesLimit;
    }

    @PrimaryKey
    @AutoIncrement
    @Column(name = "workshop_id")
    private int workshopId;

    @ForeignKey(entity = ConferenceDay.class, column = "conference_day_id")
    @Column(name = "conference_day_id")
    private int conferenceDayId;

    @Column(name = "name")
    private String name;

    @Column(name = "description")
    private String description;

    @Column(name = "price")
    private int price;

    @Column(name = "date_start")
    private Calendar dateStart;

    @Column(name = "date_end")
    private Calendar dateEnd;

    @Column(name = "attendees_limit")
    private int attendeesLimit;



}
