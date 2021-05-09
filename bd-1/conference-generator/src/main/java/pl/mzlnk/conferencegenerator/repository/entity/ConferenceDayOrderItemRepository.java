package pl.mzlnk.conferencegenerator.repository.entity;

import pl.mzlnk.conferencegenerator.model.entity.EntityType;
import pl.mzlnk.conferencegenerator.model.entity.order.ConferenceDayOrderItem;

class ConferenceDayOrderItemRepository extends BaseEntityRepository<ConferenceDayOrderItem> {

    ConferenceDayOrderItemRepository() {
        super(EntityType.CONFERENCE_DAY_ORDER_ITEM);
    }

}
