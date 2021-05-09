package pl.mzlnk.conferencegenerator.repository.entity;

import pl.mzlnk.conferencegenerator.model.entity.EntityType;
import pl.mzlnk.conferencegenerator.model.entity.order.Buyer;

class BuyerRepository extends BaseEntityRepository<Buyer> {

    BuyerRepository() {
        super(EntityType.BUYER);
    }

}
