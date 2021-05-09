package pl.mzlnk.conferencegenerator.generator.entity;

import pl.mzlnk.conferencegenerator.model.entity.EntityType;
import pl.mzlnk.conferencegenerator.model.entity.conference.ConferenceDay;
import pl.mzlnk.conferencegenerator.model.entity.conference.ConferenceDayPrice;
import pl.mzlnk.conferencegenerator.repository.data.DataRepositories;
import pl.mzlnk.conferencegenerator.repository.entity.EntityRepositories;

import java.util.Calendar;

import static pl.mzlnk.conferencegenerator.utils.RandomUtil.r;

class ConferenceDayPriceGenerator extends BaseEntityGenerator {

    ConferenceDayPriceGenerator(EntityRepositories entityRepositories, DataRepositories dataRepositories) {
        super(EntityType.CONFERENCE_DAY_PRICE, entityRepositories, dataRepositories);
    }

    @Override
    public void generate() {
        var conferenceDayRepository = entityRepositories.<ConferenceDay>getRepository(EntityType.CONFERENCE_DAY);
        var conferenceDayPriceRepository = entityRepositories.<ConferenceDayPrice>getRepository(EntityType.CONFERENCE_DAY_PRICE);

        final String discountDescription = "Znizka {discount}%";
        final String baseDescription = "Cena bazowa";

        conferenceDayRepository
                .findAll()
                .forEach(conferenceDay -> {
                    int prices = r.nextInt(4) + 1;
                    int baseValue = r.nextInt(200);

                    for(int i = 0; i < prices; i++) {
                        Calendar expirationDate = (Calendar) conferenceDay.getDate().clone();
                        if(i > 0) {
                            expirationDate.add(Calendar.DAY_OF_MONTH, -7 * i);
                        }

                        String description = (i != 0 ? discountDescription.replace("{discount}", String.valueOf(i * 10)) : baseDescription);

                        ConferenceDayPrice entity = new ConferenceDayPrice(
                                id++,
                                conferenceDay.getConferenceDayId(),
                                (int) (baseValue * (1 - 0.1 * i)),
                                description,
                                expirationDate
                                );

                        conferenceDayPriceRepository.createOrUpdate(entity);
                    }
                });

        System.out.println("generated " + conferenceDayPriceRepository.findAll().size() + " conference day price entities");
    }

}
