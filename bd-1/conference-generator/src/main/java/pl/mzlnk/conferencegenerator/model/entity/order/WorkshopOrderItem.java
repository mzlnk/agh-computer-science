package pl.mzlnk.conferencegenerator.model.entity.order;

import lombok.Builder;
import lombok.Getter;
import pl.mzlnk.conferencegenerator.model.entity.BaseEntity;
import pl.mzlnk.conferencegenerator.model.entity.EntityType;
import pl.mzlnk.conferencegenerator.generator.sql.utils.annotation.*;
import pl.mzlnk.conferencegenerator.model.entity.conference.Workshop;

@Getter
@TableOrder(4)
@Table(name = "workshop_order_item")
public class WorkshopOrderItem extends BaseEntity {

    @Builder
    public WorkshopOrderItem(int workshopOrderItemId, int conferenceDayOrderId, int workshopId) {
        super(EntityType.WORKSHOP_ORDER_ITEM, workshopOrderItemId);

        this.workshopOrderItemId = workshopOrderItemId;
        this.conferenceDayOrderId = conferenceDayOrderId;
        this.workshopId = workshopId;
    }

    @PrimaryKey
    @AutoIncrement
    @Column(name = "workshop_order_item_id")
    private int workshopOrderItemId;

    @ForeignKey(entity = ConferenceDayOrderItem.class, column = "conference_day_order_item_id")
    @Column(name = "conference_day_order_item_id")
    private int conferenceDayOrderId;

    @ForeignKey(entity = Workshop.class, column = "workshop_id")
    @Column(name = "workshop_id")
    private int workshopId;

}
