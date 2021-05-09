package pl.mzlnk.conferencegenerator.generator.entity;

import pl.mzlnk.conferencegenerator.model.entity.EntityType;
import pl.mzlnk.conferencegenerator.model.entity.conference.ConferenceDay;
import pl.mzlnk.conferencegenerator.model.entity.order.ConferenceDayOrderItem;
import pl.mzlnk.conferencegenerator.model.entity.order.Order;
import pl.mzlnk.conferencegenerator.repository.data.DataRepositories;
import pl.mzlnk.conferencegenerator.repository.entity.EntityRepositories;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static pl.mzlnk.conferencegenerator.utils.RandomUtil.r;

class ConferenceDayOrderItemGenerator extends BaseEntityGenerator {

    ConferenceDayOrderItemGenerator(EntityRepositories entityRepositories, DataRepositories dataRepositories) {
        super(EntityType.CONFERENCE_DAY_ORDER_ITEM, entityRepositories, dataRepositories);
    }

    @Override
    public void generate() {
        var conferenceDayOrderItemRepository = entityRepositories.<ConferenceDayOrderItem>getRepository(EntityType.CONFERENCE_DAY_ORDER_ITEM);
        var orderRepository = entityRepositories.<Order>getRepository(EntityType.ORDER);
        var conferenceDayRepository = entityRepositories.<ConferenceDay>getRepository(EntityType.CONFERENCE_DAY);

        final int conferenceDaySize = conferenceDayRepository.findAll().size();

        orderRepository
                .findAll()
                .forEach(order -> {
                    int conferenceDays = r.nextInt(7) + 1;

                    List<ConferenceDay> conferences = conferenceDayRepository
                            .findAll()
                            .stream()
                            .filter(cd -> cd.getDate().after(order.getDate()))
                            .collect(Collectors.toList());

                    Collections.shuffle(conferences);

                    conferences
                            .stream()
                            .limit(conferenceDays)
                            .map(ConferenceDay::getConferenceDayId)
                            .forEach(conferenceDayId -> {
                                conferenceDayOrderItemRepository.createOrUpdate(new ConferenceDayOrderItem(id++, order.getOrderId(), conferenceDayId));
                            });
                });

        System.out.println("generated: " + conferenceDayOrderItemRepository.findAll().size() + " conference day order item entities");
    }

}
