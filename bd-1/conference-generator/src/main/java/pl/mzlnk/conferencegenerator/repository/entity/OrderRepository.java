package pl.mzlnk.conferencegenerator.repository.entity;

import pl.mzlnk.conferencegenerator.model.entity.EntityType;
import pl.mzlnk.conferencegenerator.model.entity.order.Order;

class OrderRepository extends BaseEntityRepository<Order> {

    OrderRepository() {
        super(EntityType.ORDER);
    }

}
