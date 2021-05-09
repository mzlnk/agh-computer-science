package pl.mzlnk.conferencegenerator.generator.entity;

import pl.mzlnk.conferencegenerator.model.entity.EntityType;
import pl.mzlnk.conferencegenerator.model.entity.attendee.Attendee;
import pl.mzlnk.conferencegenerator.model.entity.attendee.ConferenceDayAttendee;
import pl.mzlnk.conferencegenerator.model.entity.order.ConferenceDayOrderItem;
import pl.mzlnk.conferencegenerator.repository.data.DataRepositories;
import pl.mzlnk.conferencegenerator.repository.entity.EntityRepositories;

import java.util.HashSet;
import java.util.Set;

import static pl.mzlnk.conferencegenerator.utils.RandomUtil.r;

class ConferenceDayAttendeeGenerator extends BaseEntityGenerator {

    ConferenceDayAttendeeGenerator(EntityRepositories entityRepositories, DataRepositories dataRepositories) {
        super(EntityType.CONFERENCE_DAY_ATTENDEE, entityRepositories, dataRepositories);
    }

    @Override
    public void generate() {
        var conferenceDayOrderItemRepository = entityRepositories.<ConferenceDayOrderItem>getRepository(EntityType.CONFERENCE_DAY_ORDER_ITEM);
        var conferenceDayAttendeeRepository = entityRepositories.<ConferenceDayAttendee>getRepository(EntityType.CONFERENCE_DAY_ATTENDEE);
        var attendeeRepository = entityRepositories.<Attendee>getRepository(EntityType.ATTENDEE);

        final int attendeeSize = attendeeRepository.findAll().size();

        conferenceDayOrderItemRepository
                .findAll()
                .forEach(conferenceDayOrderItem -> {
                    int registeredAttendees = r.nextInt(40) + 1;
                    int knownAttendees = r.nextInt(registeredAttendees) + 1;

                    Set<Integer> attendeeIds = new HashSet<>();
                    for(int i = 0; i < knownAttendees; i++) attendeeIds.add(r.nextInt(attendeeSize) + 1);

                    for(int attendeeId : attendeeIds) {
                        conferenceDayAttendeeRepository.createOrUpdate(new ConferenceDayAttendee(this.id++, attendeeId, conferenceDayOrderItem.getConferenceDayOrderItemId()));
                    }

                    for(int i = 0; i < registeredAttendees - attendeeIds.size(); i++) {
                        conferenceDayAttendeeRepository.createOrUpdate(new ConferenceDayAttendee(this.id++, -1, conferenceDayOrderItem.getConferenceDayOrderItemId()));
                    }
                });

        System.out.println("generated " + conferenceDayAttendeeRepository.findAll().size() + " conference day attendee entities");
    }

}
