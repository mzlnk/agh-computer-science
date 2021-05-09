package pl.mzlnk.conferencegenerator.model.entity.attendee;

import lombok.Builder;
import lombok.Getter;
import pl.mzlnk.conferencegenerator.model.entity.BaseEntity;
import pl.mzlnk.conferencegenerator.model.entity.EntityType;
import pl.mzlnk.conferencegenerator.generator.sql.utils.annotation.*;
import pl.mzlnk.conferencegenerator.model.entity.order.ConferenceDayOrderItem;

@Getter
@TableOrder(4)
@Table(name = "conference_day_attendee")
public class ConferenceDayAttendee extends BaseEntity {

    @Builder
    public ConferenceDayAttendee(int conferenceDayAttendeeId, int attendeeId, int conferenceDayOrderItemId) {
        super(EntityType.CONFERENCE_DAY_ATTENDEE, conferenceDayAttendeeId);

        this.conferenceDayAttendeeId = conferenceDayAttendeeId;
        this.attendeeId = attendeeId;
        this.conferenceDayOrderItemId = conferenceDayOrderItemId;
    }

    @PrimaryKey
    @AutoIncrement
    @Column(name = "conference_day_attendee_id")
    private int conferenceDayAttendeeId;

    @ForeignKey(entity = Attendee.class, column = "attendee_id")
    @Column(name = "attendee_id")
    private int attendeeId;

    @ForeignKey(entity = ConferenceDayOrderItem.class, column = "conference_day_order_item_id")
    @Column(name = "conference_day_order_item_id")
    private int conferenceDayOrderItemId;

}
