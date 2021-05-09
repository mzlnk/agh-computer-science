package pl.mzlnk.conferencegenerator.model.entity.attendee;

import lombok.Builder;
import lombok.Getter;
import pl.mzlnk.conferencegenerator.model.entity.BaseEntity;
import pl.mzlnk.conferencegenerator.model.entity.EntityType;
import pl.mzlnk.conferencegenerator.generator.sql.utils.annotation.*;
import pl.mzlnk.conferencegenerator.model.entity.order.WorkshopOrderItem;

@Getter
@TableOrder(5)
@Table(name = "workshop_attendee")
public class WorkshopAttendee extends BaseEntity {

    @Builder
    public WorkshopAttendee(int workshopAttendeeId, int conferenceDayAttendeeId, int workshopOrderItemId) {
        super(EntityType.WORKSHOP_ATTENDEE, workshopAttendeeId);

        this.workshopAttendeeId = workshopAttendeeId;
        this.conferenceDayAttendeeId = conferenceDayAttendeeId;
        this.workshopOrderItemId = workshopOrderItemId;
    }

    @PrimaryKey
    @AutoIncrement
    @Column(name = "workshop_attendee_id")
    private int workshopAttendeeId;

    @ForeignKey(entity = ConferenceDayAttendee.class, column = "conference_day_attendee_id")
    @Column(name = "conference_day_attendee_id")
    private int conferenceDayAttendeeId;

    @ForeignKey(entity = WorkshopOrderItem.class, column = "workshop_order_item_id")
    @Column(name = "workshop_order_item_id")
    private int workshopOrderItemId;

}
