package pl.mzlnk.conferencegenerator.model.entity.order;

import lombok.Builder;
import lombok.Getter;
import pl.mzlnk.conferencegenerator.generator.sql.utils.annotation.*;
import pl.mzlnk.conferencegenerator.model.entity.BaseEntity;
import pl.mzlnk.conferencegenerator.model.entity.EntityType;
import pl.mzlnk.conferencegenerator.model.entity.conference.ConferenceDay;

@Getter
@TableOrder(3)
@Table(name = "conference_day_order_item")
public class ConferenceDayOrderItem extends BaseEntity {

    @Builder
    public ConferenceDayOrderItem(int conferenceDayOrderItemId, int orderId, int conferenceDayId) {
        super(EntityType.CONFERENCE_DAY_ORDER_ITEM, conferenceDayOrderItemId);

        this.conferenceDayOrderItemId = conferenceDayOrderItemId;
        this.orderId = orderId;
        this.conferenceDayId = conferenceDayId;
    }

    @PrimaryKey
    @AutoIncrement
    @Column(name = "conference_day_order_item_id")
    private int conferenceDayOrderItemId;

    @ForeignKey(entity = Order.class, column = "order_id")
    @Column(name = "order_id")
    private int orderId;

    @ForeignKey(entity = ConferenceDay.class, column = "conference_day_id")
    @Column(name = "conference_day_id")
    private int conferenceDayId;

}
