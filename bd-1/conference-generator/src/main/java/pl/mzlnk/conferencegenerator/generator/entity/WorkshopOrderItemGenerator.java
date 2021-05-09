package pl.mzlnk.conferencegenerator.generator.entity;

import pl.mzlnk.conferencegenerator.model.entity.EntityType;
import pl.mzlnk.conferencegenerator.model.entity.conference.ConferenceDay;
import pl.mzlnk.conferencegenerator.model.entity.conference.Workshop;
import pl.mzlnk.conferencegenerator.model.entity.order.ConferenceDayOrderItem;
import pl.mzlnk.conferencegenerator.model.entity.order.WorkshopOrderItem;
import pl.mzlnk.conferencegenerator.repository.data.DataRepositories;
import pl.mzlnk.conferencegenerator.repository.entity.EntityRepositories;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import static pl.mzlnk.conferencegenerator.utils.RandomUtil.r;

class WorkshopOrderItemGenerator extends BaseEntityGenerator {

    WorkshopOrderItemGenerator(EntityRepositories entityRepositories, DataRepositories dataRepositories) {
        super(EntityType.WORKSHOP_ORDER_ITEM, entityRepositories, dataRepositories);
    }

    @Override
    public void generate() {
        var workshopOrderItemRepository = entityRepositories.<WorkshopOrderItem>getRepository(EntityType.WORKSHOP_ORDER_ITEM);
        var conferenceDayOrderItemRepository = entityRepositories.<ConferenceDayOrderItem>getRepository(EntityType.CONFERENCE_DAY_ORDER_ITEM);
        var conferenceDayRepository = entityRepositories.<ConferenceDay>getRepository(EntityType.CONFERENCE_DAY);
        var workshopRepository = entityRepositories.<Workshop>getRepository(EntityType.WORKSHOP);

        conferenceDayOrderItemRepository
                .findAll()
                .forEach(conferenceDayOrderItem -> {
                    List<Workshop> workshops = conferenceDayRepository
                            .findById(conferenceDayOrderItem.getConferenceDayId())
                            .map(conferenceDay -> {
                                return workshopRepository
                                        .findAll()
                                        .stream()
                                        .filter(workshop -> workshop.getConferenceDayId() == conferenceDay.getConferenceDayId())
                                        .filter(workshop -> r.nextBoolean())
                                        .collect(Collectors.toList());
                            })
                            .orElse(new ArrayList<>());

                    workshops.forEach(workshop -> {
                        workshopOrderItemRepository.createOrUpdate(new WorkshopOrderItem(
                                id++,
                                conferenceDayOrderItem.getConferenceDayOrderItemId(),
                                workshop.getWorkshopId())
                        );
                    });
                });

        System.out.println("generated " + workshopOrderItemRepository.findAll().size() + " workshop order item entities");
    }

}
