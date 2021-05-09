package pl.mzlnk.conferencegenerator.repository.entity;

import pl.mzlnk.conferencegenerator.model.entity.EntityType;
import pl.mzlnk.conferencegenerator.model.entity.conference.Workshop;

class WorkshopRepository extends BaseEntityRepository<Workshop> {

    WorkshopRepository() {
        super(EntityType.WORKSHOP);
    }

}
