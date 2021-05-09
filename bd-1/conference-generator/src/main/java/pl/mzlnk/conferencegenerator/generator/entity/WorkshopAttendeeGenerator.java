package pl.mzlnk.conferencegenerator.generator.entity;

import pl.mzlnk.conferencegenerator.model.entity.EntityType;
import pl.mzlnk.conferencegenerator.model.entity.attendee.ConferenceDayAttendee;
import pl.mzlnk.conferencegenerator.model.entity.attendee.WorkshopAttendee;
import pl.mzlnk.conferencegenerator.model.entity.conference.Workshop;
import pl.mzlnk.conferencegenerator.model.entity.order.ConferenceDayOrderItem;
import pl.mzlnk.conferencegenerator.model.entity.order.WorkshopOrderItem;
import pl.mzlnk.conferencegenerator.repository.data.DataRepositories;
import pl.mzlnk.conferencegenerator.repository.entity.EntityRepositories;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import static pl.mzlnk.conferencegenerator.utils.RandomUtil.r;

class WorkshopAttendeeGenerator extends BaseEntityGenerator {

    WorkshopAttendeeGenerator(EntityRepositories entityRepositories, DataRepositories dataRepositories) {
        super(EntityType.WORKSHOP_ATTENDEE, entityRepositories, dataRepositories);
    }

    @Override
    public void generate() {
        var workshopAttendeeRepository = entityRepositories.<WorkshopAttendee>getRepository(EntityType.WORKSHOP_ATTENDEE);
        var workshopRepository = entityRepositories.<Workshop>getRepository(EntityType.WORKSHOP);
        var workshopOrderItemRepository = entityRepositories.<WorkshopOrderItem>getRepository(EntityType.WORKSHOP_ORDER_ITEM);
        var conferenceDayOrderItemRepository = entityRepositories.<ConferenceDayOrderItem>getRepository(EntityType.CONFERENCE_DAY_ORDER_ITEM);
        var conferenceDayAttendeeRepository = entityRepositories.<ConferenceDayAttendee>getRepository(EntityType.CONFERENCE_DAY_ATTENDEE);

        workshopOrderItemRepository
                .findAll()
                .forEach(workshopOrderItem -> {
                    int attendeesLimit = workshopRepository
                            .findById(workshopOrderItem.getWorkshopId())
                            .map(Workshop::getAttendeesLimit)
                            .orElse(0);

                    int registeredAttendees = (int) workshopAttendeeRepository
                            .findAll()
                            .stream()
                            .map(workshopAttendee -> workshopOrderItemRepository.findById(workshopAttendee.getWorkshopOrderItemId()))
                            .filter(Optional::isPresent)
                            .map(Optional::get)
                            .filter(wi -> wi.getWorkshopId() == workshopOrderItem.getWorkshopId())
                            .count();

                    int attendeesLeft = attendeesLimit - registeredAttendees;

                    if (attendeesLeft <= 0) {
                        return;
                    }

                    int attendees = r.nextInt(Math.min(attendeesLeft, 5)) + 1;

                    int conferenceDayOrderItemId = workshopOrderItem.getConferenceDayOrderId();

                    Workshop workshop = workshopRepository.findById(workshopOrderItem.getWorkshopId()).get();

                    List<ConferenceDayAttendee> applicableAttendees = conferenceDayAttendeeRepository
                            .findAll()
                            .stream()
                            .filter(cda -> cda.getConferenceDayOrderItemId() == conferenceDayOrderItemId)
                            .filter(cda -> {
                                return workshopAttendeeRepository
                                        .findAll()
                                        .stream()
                                        .filter(wa -> wa.getConferenceDayAttendeeId() == cda.getConferenceDayAttendeeId())
                                        .map(wa -> workshopOrderItemRepository.findById(wa.getWorkshopOrderItemId()))
                                        .filter(Optional::isPresent)
                                        .map(Optional::get)
                                        .map(woi -> workshopRepository.findById(woi.getWorkshopId()))
                                        .filter(Optional::isPresent)
                                        .map(Optional::get)
                                        .noneMatch(w -> {
                                            return (workshop.getDateStart().after(w.getDateStart()) && workshop.getDateStart().before(w.getDateEnd())
                                                    || (workshop.getDateEnd().after(w.getDateStart()) && workshop.getDateStart().before(w.getDateStart())));
                                        });
                            })
                            .collect(Collectors.toList());

                    Collections.shuffle(applicableAttendees);

                    applicableAttendees
                            .stream()
                            .limit(attendees)
                            .forEach(cda -> workshopAttendeeRepository.createOrUpdate(new WorkshopAttendee(id++, cda.getConferenceDayAttendeeId(), workshopOrderItem.getWorkshopOrderItemId())));

                });

        System.out.println("generated: " + workshopAttendeeRepository.findAll().size() + " workshop attendee entities");
    }

}
