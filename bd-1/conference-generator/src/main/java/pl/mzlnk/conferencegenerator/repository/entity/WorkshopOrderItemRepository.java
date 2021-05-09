package pl.mzlnk.conferencegenerator.repository.entity;

import pl.mzlnk.conferencegenerator.model.entity.EntityType;
import pl.mzlnk.conferencegenerator.model.entity.order.WorkshopOrderItem;

class WorkshopOrderItemRepository extends BaseEntityRepository<WorkshopOrderItem> {

    WorkshopOrderItemRepository() {
        super(EntityType.WORKSHOP_ORDER_ITEM);
    }

}
