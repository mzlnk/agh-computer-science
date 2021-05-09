package pl.mzlnk.conferencegenerator.generator.entity;

import pl.mzlnk.conferencegenerator.model.entity.EntityType;
import pl.mzlnk.conferencegenerator.model.entity.attendee.WorkshopAttendee;
import pl.mzlnk.conferencegenerator.model.entity.order.WorkshopOrderItem;
import pl.mzlnk.conferencegenerator.repository.data.DataRepositories;
import pl.mzlnk.conferencegenerator.repository.entity.EntityRepositories;

import java.util.List;
import java.util.stream.Collectors;

class CleanupGenerator extends BaseEntityGenerator {

    CleanupGenerator(EntityRepositories entityRepositories, DataRepositories dataRepositories) {
        super(null, entityRepositories, dataRepositories);
    }

    @Override
    public void generate() {
        var workshopOrderItemRepository = entityRepositories.<WorkshopOrderItem>getRepository(EntityType.WORKSHOP_ORDER_ITEM);
        var workshopAttendeeRepository = entityRepositories.<WorkshopAttendee>getRepository(EntityType.WORKSHOP_ATTENDEE);

        List<WorkshopOrderItem> emptyOrders = workshopOrderItemRepository
                .findAll()
                .stream()
                .filter(woi -> {
                    return workshopAttendeeRepository
                            .findAll()
                            .stream()
                            .noneMatch(wa -> wa.getWorkshopOrderItemId() == woi.getConferenceDayOrderId());
                })
                .collect(Collectors.toList());

        int size = emptyOrders.size();

        emptyOrders.forEach(woi -> workshopOrderItemRepository.remove(woi.getWorkshopOrderItemId()));

        System.out.println("removed " + size + " empty workshop order item entites");
    }

}
